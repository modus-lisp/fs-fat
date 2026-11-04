;;;; lib/fat32.lisp -- FAT32 reader and writer (pure CL, no FFI).
;;;;
;;;; Reads and writes a FAT32 volume held in a byte vector: the whole partition
;;;; as mkfs.vfat -F 32 writes it, or a slice of a disk image.  Writes change that
;;;; vector in place, so the caller saves it.  VFAT long file
;;;; names are used when present and their checksum matches the 8.3 name;
;;;; otherwise the 8.3 name is shown, with the NT lowercase flags honoured.
;;;; Deleted entries and the volume label are skipped; "." and ".." are not
;;;; listed.  Lookup is case-insensitive, as FAT is.
;;;;
;;;; 8.3 names are decoded one char per byte, which is right for ASCII and wrong
;;;; for OEM code-page 437 characters above 127.  LFN code units become chars
;;;; directly; surrogate pairs are not combined.
;;;;
;;;; Not here yet: FAT12/16, exFAT, and a sector-reading seam for block devices
;;;; (the library needs the whole volume in memory).
;;;;
;;;; Public API (reading):
;;;;   (fat32-open bytes)          -> volume.  An ERROR if BYTES is not FAT32.
;;;;   (fat32-lookup vol path)     -> entry, or NIL.  PATH is "/"-separated; ""
;;;;                                  and "/" name the root.
;;;;   (fat32-directory vol path)  -> entries of the directory at PATH, or NIL if
;;;;                                  PATH does not name a directory.
;;;;   (fat32-read-file vol path)  -> fresh byte vector with the file's contents,
;;;;                                  or NIL if PATH does not name a regular file.
;;;;
;;;; An entry is (NAME DIRP SIZE FIRST-CLUSTER): NAME a string, DIRP true for a
;;;; directory, SIZE in bytes (0 for directories), FIRST-CLUSTER where the
;;;; cluster chain starts (0 for an empty file).
;;;;
;;;; Public API (writing, below): fat32-write-file, fat32-mkdir, fat32-delete,
;;;; fat32-rename, fat32-sync.  Each takes the volume and changes it.
;;;;
;;;; Devices (below): (fat32-open bytes &optional device) without a device writes the
;;;; bytes directly.  With one, every read and write goes through it: device-read,
;;;; device-write and device-flush.  make-fat32-device gives a simulated disk, which
;;;; the crash tests use to stop the library at any write or flush.
;;;;
;;;; Style follows lib/tar.lisp: plain MAKE-ARRAY and explicit loops, because
;;;; tar.lisp records that the bare-metal image wedges on a MAKE-ARRAY
;;;; :element-type keyword.  Byte vectors here are therefore generic vectors
;;;; holding byte values.

;;; ---- byte access -------------------------------------------------------------

;;;; The package.  The public API is exported; the %-prefixed helpers are internal
;;;; to it, and the test harnesses reach them as FS-FAT::%NAME.
(defpackage #:fs-fat
  (:use #:cl)
  (:export #:fat32-open #:fat32-open-device #:fat32-lookup #:fat32-directory
           #:fat32-read-file #:fat32-write-file #:fat32-mkdir #:fat32-delete
           #:fat32-rename #:fat32-sync #:fat32-format
           #:make-fat32-device #:make-fat32-block-device #:device-size
           #:device-read #:device-write #:device-flush
           #:device-crash-writes #:device-dead-p #:device-events
           #:exfat-open #:exfat-lookup #:exfat-directory #:exfat-read-file
           #:exfat-write-file #:exfat-mkdir #:exfat-delete #:exfat-rename
           #:exfat-sync))

(in-package #:fs-fat)

;; Used before their definitions below, so a source LOAD does not warn about them.
(declaim (ftype function device-read device-size %fat-copy-into))

(defun %fat-u8 (bytes off)
  (when (or (< off 0) (>= off (length bytes)))
    (error "fat32: read past end of volume at byte ~a" off))
  (aref bytes off))

(defun %fat-le (bytes off n)
  "Unsigned little-endian integer of N bytes at OFF."
  (let ((v 0) (i (- n 1)))
    (loop
      (when (< i 0) (return))
      (setq v (+ (* v 256) (%fat-u8 bytes (+ off i))))
      (setq i (- i 1)))
    v))

;;; ---- volume ------------------------------------------------------------------
;;; A volume is the list (BYTES BPS SPC FAT-START DATA-START ROOT-CLUSTER NCLUSTERS
;;; NFATS FATSZ FSINFO DEVICE).  FAT-START, DATA-START, FATSZ (one FAT copy) and
;;; FSINFO (the FSInfo sector, or NIL) are byte offsets.  DEVICE is NIL or a device.

(defun %fat-bytes (v) (nth 0 v))
(defun %fat-bps (v) (nth 1 v))
(defun %fat-spc (v) (nth 2 v))
(defun %fat-fat-start (v) (nth 3 v))
(defun %fat-data-start (v) (nth 4 v))
(defun %fat-root (v) (nth 5 v))
(defun %fat-nclusters (v) (nth 6 v))
(defun %fat-nfats (v) (nth 7 v))
(defun %fat-fatsz (v) (nth 8 v))
(defun %fat-fsinfo (v) (nth 9 v))
(defun %fat-device (v) (nth 10 v))

(defun %fat-cluster-bytes (v) (* (%fat-bps v) (%fat-spc v)))

(defun %fat-cluster-offset (v c)
  (+ (%fat-data-start v) (* (- c 2) (%fat-cluster-bytes v))))

(defun %fat-read (v off n)
  "N bytes at OFF, as a fresh vector.  Reads go through the device when there is one,
   so a block device need not hold the whole volume in memory."
  (let ((dev (%fat-device v)))
    (if dev
        (device-read dev off n)
        (subseq (%fat-bytes v) off (+ off n)))))

(defun %fat-read-le (v off n)
  (%fat-le (%fat-read v off n) 0 n))

(defun %fat-next (v c)
  "The FAT entry for cluster C: the next cluster, or an end or bad marker."
  (logand (%fat-read-le v (+ (%fat-fat-start v) (* c 4)) 4) #x0FFFFFFF))

(defun fat32-open (bytes &optional device)
  "A volume over BYTES.  DEVICE, when given, is the disk BYTES is the view of: writes
   then go through the device, and barriers flush it (see the devices section).  For a
   block device BYTES need only hold the boot sector."
  (let ((len (if device (device-size device) (length bytes))))
    (when (< len 512)
      (error "fat32: volume is ~a bytes, too short for a boot sector" len))
    (unless (and (= (%fat-u8 bytes 510) #x55) (= (%fat-u8 bytes 511) #xAA))
      (error "fat32: no boot sector signature"))
    (let* ((bps (%fat-le bytes 11 2))
           (spc (%fat-u8 bytes 13))
           (rsvd (%fat-le bytes 14 2))
           (nfats (%fat-u8 bytes 16))
           (rootent (%fat-le bytes 17 2))
           (tot16 (%fat-le bytes 19 2))
           (fatsz16 (%fat-le bytes 22 2))
           (tot32 (%fat-le bytes 32 4))
           (fatsz32 (%fat-le bytes 36 4))
           (rootclus (%fat-le bytes 44 4))
           (total (if (= tot16 0) tot32 tot16))
           (data-sectors (- total (+ rsvd (* nfats fatsz32))))
           (nclusters (if (> spc 0) (floor data-sectors spc) 0)))
      (unless (or (= bps 512) (= bps 1024) (= bps 2048) (= bps 4096))
        (error "fat32: bytes per sector ~a is not 512, 1024, 2048 or 4096" bps))
      (unless (and (> spc 0) (<= spc 128) (= 0 (logand spc (- spc 1))))
        (error "fat32: sectors per cluster ~a is not a power of two up to 128" spc))
      (unless (and (= rootent 0) (= fatsz16 0))
        (error "fat32: root entry count or FATSz16 is nonzero -- FAT12/16, not FAT32"))
      (unless (and (> fatsz32 0) (> rsvd 0) (>= nfats 1))
        (error "fat32: reserved sectors, FAT count or FAT size is zero"))
      (unless (>= rootclus 2)
        (error "fat32: root cluster ~a is invalid" rootclus))
      (when (> (* total bps) len)
        (error "fat32: volume declares ~a bytes but only ~a are present" (* total bps) len))
      (when (< nclusters 65525)
        (error "fat32: ~a data clusters -- FAT32 needs at least 65525" nclusters))
      (when (< (* fatsz32 bps) (* 4 (+ nclusters 2)))
        (error "fat32: FAT is too small for ~a clusters" nclusters))
      (when (> rootclus (+ nclusters 1))
        (error "fat32: root cluster ~a is past the last cluster" rootclus))
      (let ((fsinfo (%fat-le bytes 48 2)))
        (list bytes bps spc (* rsvd bps) (* (+ rsvd (* nfats fatsz32)) bps)
              rootclus nclusters nfats (* fatsz32 bps)
              (if (and (> fsinfo 0) (< fsinfo rsvd)) (* fsinfo bps) nil)
              device)))))

;;; ---- cluster chains and file data ----------------------------------------------

(defun %fat-chain (v first)
  "The clusters of the chain starting at FIRST, in order.  A FIRST below 2 is an
   empty chain (an empty file)."
  (let ((acc nil) (c first) (n 0) (limit (%fat-nclusters v)))
    (when (>= c 2)
      (loop
        (when (or (< c 2) (> c (+ limit 1)))
          (error "fat32: cluster ~a is outside the data area" c))
        (when (>= n limit)
          (error "fat32: cluster chain is longer than the volume"))
        (setq acc (cons c acc))
        (setq n (+ n 1))
        (setq c (%fat-next v c))
        (when (>= c #x0FFFFFF8) (return))))
    (nreverse acc)))

(defun %fat-read-clusters (v clusters size)
  "Fresh vector holding the first SIZE bytes of the clusters CLUSTERS.  SIZE is
   checked against the chain's capacity before anything is allocated: a size
   field taken from a corrupt directory entry can ask for gigabytes."
  (let ((cs (%fat-cluster-bytes v))
        (capacity (* (length clusters) (%fat-cluster-bytes v))))
    (when (> size capacity)
      (error "fat32: chain holds ~a bytes, ~a expected" capacity size))
    (let ((out (make-array size :element-type (quote (unsigned-byte 8))))
          (pos 0))
      (dolist (c clusters)
        (let ((data (%fat-read v (%fat-cluster-offset v c) cs)))
          (dotimes (i cs)
            (when (>= pos size) (return))
            (setf (aref out pos) (aref data i))
            (setq pos (+ pos 1)))))
      out)))

;;; ---- names -------------------------------------------------------------------

(defun %fat-string (chars)
  "A fresh string of the characters in the list CHARS."
  (let* ((n (length chars)) (out (make-string n)) (i 0))
    (dolist (ch chars)
      (setf (char out i) ch)
      (setq i (+ i 1)))
    out))

(defun %default-upcase-hex ()
  "The up-case table exFAT volumes from mkfs.exfat carry, as hex: the compressed table
   of 2918 units.  FAT has no table on disk, so FAT uses this one.  It is generated
   from Unicode case data, and is checked against the checksum recorded with it."
  "00000100020003000400050006000700080009000a000b000c000d000e000f0010001100120013001400150016001700180019001a001b001c001d001e001f0020002100220023002400250026002700280029002a002b002c002d002e002f0030003100320033003400350036003700380039003a003b003c003d003e003f0040004100420043004400450046004700480049004a004b004c004d004e004f0050005100520053005400550056005700580059005a005b005c005d005e005f0060004100420043004400450046004700480049004a004b004c004d004e004f0050005100520053005400550056005700580059005a007b007c007d007e007f0080008100820083008400850086008700880089008a008b008c008d008e008f0090009100920093009400950096009700980099009a009b009c009d009e009f00a000a100a200a300a400a500a600a700a800a900aa00ab00ac00ad00ae00af00b000b100b200b300b400b500b600b700b800b900ba00bb00bc00bd00be00bf00c000c100c200c300c400c500c600c700c800c900ca00cb00cc00cd00ce00cf00d000d100d200d300d400d500d600d700d800d900da00db00dc00dd00de00df00c000c100c200c300c400c500c600c700c800c900ca00cb00cc00cd00ce00cf00d000d100d200d300d400d500d600f700d800d900da00db00dc00dd00de00780100010001020102010401040106010601080108010a010a010c010c010e010e0110011001120112011401140116011601180118011a011a011c011c011e011e0120012001220122012401240126012601280128012a012a012c012c012e012e01300131013201320134013401360136013801390139013b013b013d013d013f013f014101410143014301450145014701470149014a014a014c014c014e014e0150015001520152015401540156015601580158015a015a015c015c015e015e0160016001620162016401640166016601680168016a016a016c016c016e016e01700170017201720174017401760176017801790179017b017b017d017d017f0143028101820182018401840186018701870189018a018b018b018d018e018f0190019101910193019401f60196019701980198013d029b019c019d0120029f01a001a001a201a201a401a401a601a701a701a901aa01ab01ac01ac01ae01af01af01b101b201b301b301b501b501b701b801b801ba01bb01bc01bc01be01f701c001c101c201c301c401c501c401c701c801c701ca01cb01ca01cd01cd01cf01cf01d101d101d301d301d501d501d701d701d901d901db01db018e01de01de01e001e001e201e201e401e401e601e601e801e801ea01ea01ec01ec01ee01ee01f001f101f201f101f401f401f601f701f801f801fa01fa01fc01fc01fe01fe0100020002020202020402040206020602080208020a020a020c020c020e020e0210021002120212021402140216021602180218021a021a021c021c021e021e0220022102220222022402240226022602280228022a022a022c022c022e022e023002300232023202340235023602370238023902652c3b023b023d02662c3f0240024102410243024402450246024602480248024a024a024c024c024e024e0250025102520281018601550289018a0158028f015a0290015c025d025e025f0293016102620294016402650266026702970196016a02622c6c026d026e029c01700271029d01730274029f0176027702780279027a027b027c02642c7e027f02a60181028202a9018402850286028702ae014402b101b20145028d028e028f0290029102b70193029402950296029702980299029a029b029c029d029e029f02a002a102a202a302a402a502a602a702a802a902aa02ab02ac02ad02ae02af02b002b102b202b302b402b502b602b702b802b902ba02bb02bc02bd02be02bf02c002c102c202c302c402c502c602c702c802c902ca02cb02cc02cd02ce02cf02d002d102d202d302d402d502d602d702d802d902da02db02dc02dd02de02df02e002e102e202e302e402e502e602e702e802e902ea02eb02ec02ed02ee02ef02f002f102f202f302f402f502f602f702f802f902fa02fb02fc02fd02fe02ff0200030103020303030403050306030703080309030a030b030c030d030e030f0310031103120313031403150316031703180319031a031b031c031d031e031f0320032103220323032403250326032703280329032a032b032c032d032e032f0330033103320333033403350336033703380339033a033b033c033d033e033f0340034103420343034403450346034703480349034a034b034c034d034e034f0350035103520353035403550356035703580359035a035b035c035d035e035f0360036103620363036403650366036703680369036a036b036c036d036e036f0370037103720373037403750376037703780379037a03fd03fe03ff037e037f0380038103820383038403850386038703880389038a038b038c038d038e038f0390039103920393039403950396039703980399039a039b039c039d039e039f03a003a103a203a303a403a503a603a703a803a903aa03ab038603880389038a03b0039103920393039403950396039703980399039a039b039c039d039e039f03a003a103a303a303a403a503a603a703a803a903aa03ab038c038e038f03cf03d003d103d203d303d403d503d603d703d803d803da03da03dc03dc03de03de03e003e003e203e203e403e403e603e603e803e803ea03ea03ec03ec03ee03ee03f003f103f903f303f403f503f603f703f703f903fa03fa03fc03fd03fe03ff0300040104020403040404050406040704080409040a040b040c040d040e040f0410041104120413041404150416041704180419041a041b041c041d041e041f0420042104220423042404250426042704280429042a042b042c042d042e042f0410041104120413041404150416041704180419041a041b041c041d041e041f0420042104220423042404250426042704280429042a042b042c042d042e042f0400040104020403040404050406040704080409040a040b040c040d040e040f0460046004620462046404640466046604680468046a046a046c046c046e046e0470047004720472047404740476047604780478047a047a047c047c047e047e0480048004820483048404850486048704880489048a048a048c048c048e048e0490049004920492049404940496049604980498049a049a049c049c049e049e04a004a004a204a204a404a404a604a604a804a804aa04aa04ac04ac04ae04ae04b004b004b204b204b404b404b604b604b804b804ba04ba04bc04bc04be04be04c004c104c104c304c304c504c504c704c704c904c904cb04cb04cd04cd04c004d004d004d204d204d404d404d604d604d804d804da04da04dc04dc04de04de04e004e004e204e204e404e404e604e604e804e804ea04ea04ec04ec04ee04ee04f004f004f204f204f404f404f604f604f804f804fa04fa04fc04fc04fe04fe0400050005020502050405040506050605080508050a050a050c050c050e050e0510051005120512051405150516051705180519051a051b051c051d051e051f0520052105220523052405250526052705280529052a052b052c052d052e052f0530053105320533053405350536053705380539053a053b053c053d053e053f0540054105420543054405450546054705480549054a054b054c054d054e054f0550055105520553055405550556055705580559055a055b055c055d055e055f0560053105320533053405350536053705380539053a053b053c053d053e053f0540054105420543054405450546054705480549054a054b054c054d054e054f055005510552055305540555055605fffff617632c7e1d7f1d801d811d821d831d841d851d861d871d881d891d8a1d8b1d8c1d8d1d8e1d8f1d901d911d921d931d941d951d961d971d981d991d9a1d9b1d9c1d9d1d9e1d9f1da01da11da21da31da41da51da61da71da81da91daa1dab1dac1dad1dae1daf1db01db11db21db31db41db51db61db71db81db91dba1dbb1dbc1dbd1dbe1dbf1dc01dc11dc21dc31dc41dc51dc61dc71dc81dc91dca1dcb1dcc1dcd1dce1dcf1dd01dd11dd21dd31dd41dd51dd61dd71dd81dd91dda1ddb1ddc1ddd1dde1ddf1de01de11de21de31de41de51de61de71de81de91dea1deb1dec1ded1dee1def1df01df11df21df31df41df51df61df71df81df91dfa1dfb1dfc1dfd1dfe1dff1d001e001e021e021e041e041e061e061e081e081e0a1e0a1e0c1e0c1e0e1e0e1e101e101e121e121e141e141e161e161e181e181e1a1e1a1e1c1e1c1e1e1e1e1e201e201e221e221e241e241e261e261e281e281e2a1e2a1e2c1e2c1e2e1e2e1e301e301e321e321e341e341e361e361e381e381e3a1e3a1e3c1e3c1e3e1e3e1e401e401e421e421e441e441e461e461e481e481e4a1e4a1e4c1e4c1e4e1e4e1e501e501e521e521e541e541e561e561e581e581e5a1e5a1e5c1e5c1e5e1e5e1e601e601e621e621e641e641e661e661e681e681e6a1e6a1e6c1e6c1e6e1e6e1e701e701e721e721e741e741e761e761e781e781e7a1e7a1e7c1e7c1e7e1e7e1e801e801e821e821e841e841e861e861e881e881e8a1e8a1e8c1e8c1e8e1e8e1e901e901e921e921e941e941e961e971e981e991e9a1e9b1e9c1e9d1e9e1e9f1ea01ea01ea21ea21ea41ea41ea61ea61ea81ea81eaa1eaa1eac1eac1eae1eae1eb01eb01eb21eb21eb41eb41eb61eb61eb81eb81eba1eba1ebc1ebc1ebe1ebe1ec01ec01ec21ec21ec41ec41ec61ec61ec81ec81eca1eca1ecc1ecc1ece1ece1ed01ed01ed21ed21ed41ed41ed61ed61ed81ed81eda1eda1edc1edc1ede1ede1ee01ee01ee21ee21ee41ee41ee61ee61ee81ee81eea1eea1eec1eec1eee1eee1ef01ef01ef21ef21ef41ef41ef61ef61ef81ef81efa1efb1efc1efd1efe1eff1e081f091f0a1f0b1f0c1f0d1f0e1f0f1f081f091f0a1f0b1f0c1f0d1f0e1f0f1f181f191f1a1f1b1f1c1f1d1f161f171f181f191f1a1f1b1f1c1f1d1f1e1f1f1f281f291f2a1f2b1f2c1f2d1f2e1f2f1f281f291f2a1f2b1f2c1f2d1f2e1f2f1f381f391f3a1f3b1f3c1f3d1f3e1f3f1f381f391f3a1f3b1f3c1f3d1f3e1f3f1f481f491f4a1f4b1f4c1f4d1f461f471f481f491f4a1f4b1f4c1f4d1f4e1f4f1f501f591f521f5b1f541f5d1f561f5f1f581f591f5a1f5b1f5c1f5d1f5e1f5f1f681f691f6a1f6b1f6c1f6d1f6e1f6f1f681f691f6a1f6b1f6c1f6d1f6e1f6f1fba1fbb1fc81fc91fca1fcb1fda1fdb1ff81ff91fea1feb1ffa1ffb1f7e1f7f1f881f891f8a1f8b1f8c1f8d1f8e1f8f1f881f891f8a1f8b1f8c1f8d1f8e1f8f1f981f991f9a1f9b1f9c1f9d1f9e1f9f1f981f991f9a1f9b1f9c1f9d1f9e1f9f1fa81fa91faa1fab1fac1fad1fae1faf1fa81fa91faa1fab1fac1fad1fae1faf1fb81fb91fb21fbc1fb41fb51fb61fb71fb81fb91fba1fbb1fbc1fbd1fbe1fbf1fc01fc11fc21fc31fc41fc51fc61fc71fc81fc91fca1fcb1fc31fcd1fce1fcf1fd81fd91fd21fd31fd41fd51fd61fd71fd81fd91fda1fdb1fdc1fdd1fde1fdf1fe81fe91fe21fe31fe41fec1fe61fe71fe81fe91fea1feb1fec1fed1fee1fef1ff01ff11ff21ff31ff41ff51ff61ff71ff81ff91ffa1ffb1ff31ffd1ffe1fff1f00200120022003200420052006200720082009200a200b200c200d200e200f2010201120122013201420152016201720182019201a201b201c201d201e201f2020202120222023202420252026202720282029202a202b202c202d202e202f2030203120322033203420352036203720382039203a203b203c203d203e203f2040204120422043204420452046204720482049204a204b204c204d204e204f2050205120522053205420552056205720582059205a205b205c205d205e205f2060206120622063206420652066206720682069206a206b206c206d206e206f2070207120722073207420752076207720782079207a207b207c207d207e207f2080208120822083208420852086208720882089208a208b208c208d208e208f2090209120922093209420952096209720982099209a209b209c209d209e209f20a020a120a220a320a420a520a620a720a820a920aa20ab20ac20ad20ae20af20b020b120b220b320b420b520b620b720b820b920ba20bb20bc20bd20be20bf20c020c120c220c320c420c520c620c720c820c920ca20cb20cc20cd20ce20cf20d020d120d220d320d420d520d620d720d820d920da20db20dc20dd20de20df20e020e120e220e320e420e520e620e720e820e920ea20eb20ec20ed20ee20ef20f020f120f220f320f420f520f620f720f820f920fa20fb20fc20fd20fe20ff2000210121022103210421052106210721082109210a210b210c210d210e210f2110211121122113211421152116211721182119211a211b211c211d211e211f2120212121222123212421252126212721282129212a212b212c212d212e212f2130213121322133213421352136213721382139213a213b213c213d213e213f2140214121422143214421452146214721482149214a214b214c214d2132214f2150215121522153215421552156215721582159215a215b215c215d215e215f2160216121622163216421652166216721682169216a216b216c216d216e216f2160216121622163216421652166216721682169216a216b216c216d216e216f2180218121822183218321ffff4b03b624b724b824b924ba24bb24bc24bd24be24bf24c024c124c224c324c424c524c624c724c824c924ca24cb24cc24cd24ce24cf24ffff4607002c012c022c032c042c052c062c072c082c092c0a2c0b2c0c2c0d2c0e2c0f2c102c112c122c132c142c152c162c172c182c192c1a2c1b2c1c2c1d2c1e2c1f2c202c212c222c232c242c252c262c272c282c292c2a2c2b2c2c2c2d2c2e2c5f2c602c602c622c632c642c652c662c672c672c692c692c6b2c6b2c6d2c6e2c6f2c702c712c722c732c742c752c752c772c782c792c7a2c7b2c7c2c7d2c7e2c7f2c802c802c822c822c842c842c862c862c882c882c8a2c8a2c8c2c8c2c8e2c8e2c902c902c922c922c942c942c962c962c982c982c9a2c9a2c9c2c9c2c9e2c9e2ca02ca02ca22ca22ca42ca42ca62ca62ca82ca82caa2caa2cac2cac2cae2cae2cb02cb02cb22cb22cb42cb42cb62cb62cb82cb82cba2cba2cbc2cbc2cbe2cbe2cc02cc02cc22cc22cc42cc42cc62cc62cc82cc82cca2cca2ccc2ccc2cce2cce2cd02cd02cd22cd22cd42cd42cd62cd62cd82cd82cda2cda2cdc2cdc2cde2cde2ce02ce02ce22ce22ce42ce52ce62ce72ce82ce92cea2ceb2cec2ced2cee2cef2cf02cf12cf22cf32cf42cf52cf62cf72cf82cf92cfa2cfb2cfc2cfd2cfe2cff2ca010a110a210a310a410a510a610a710a810a910aa10ab10ac10ad10ae10af10b010b110b210b310b410b510b610b710b810b910ba10bb10bc10bd10be10bf10c010c110c210c310c410c510ffff1bd221ff22ff23ff24ff25ff26ff27ff28ff29ff2aff2bff2cff2dff2eff2fff30ff31ff32ff33ff34ff35ff36ff37ff38ff39ff3aff5bff5cff5dff5eff5fff60ff61ff62ff63ff64ff65ff66ff67ff68ff69ff6aff6bff6cff6dff6eff6fff70ff71ff72ff73ff74ff75ff76ff77ff78ff79ff7aff7bff7cff7dff7eff7fff80ff81ff82ff83ff84ff85ff86ff87ff88ff89ff8aff8bff8cff8dff8eff8fff90ff91ff92ff93ff94ff95ff96ff97ff98ff99ff9aff9bff9cff9dff9eff9fffa0ffa1ffa2ffa3ffa4ffa5ffa6ffa7ffa8ffa9ffaaffabffacffadffaeffafffb0ffb1ffb2ffb3ffb4ffb5ffb6ffb7ffb8ffb9ffbaffbbffbcffbdffbeffbfffc0ffc1ffc2ffc3ffc4ffc5ffc6ffc7ffc8ffc9ffcaffcbffccffcdffceffcfffd0ffd1ffd2ffd3ffd4ffd5ffd6ffd7ffd8ffd9ffdaffdbffdcffddffdeffdfffe0ffe1ffe2ffe3ffe4ffe5ffe6ffe7ffe8ffe9ffeaffebffecffedffeeffeffff0fff1fff2fff3fff4fff5fff6fff7fff8fff9fffafffbfffcfffdfffeffffff")

(defun %hex-bytes (hex)
  (let* ((n (floor (length hex) 2))
         (out (make-array n)))
    (dotimes (i n out)
      (setf (aref out i) (parse-integer hex :start (* 2 i) :end (+ (* 2 i) 2) :radix 16)))))

(defun %upcase-decode (bytes)
  "The up-case table BYTES as a vector of 65536 units.  Its entries are 16-bit units in
   order, except that 0xFFFF followed by a count means that many units map to
   themselves.  Units the table does not cover map to themselves too."
  (let* ((n (floor (length bytes) 2))
         (table (make-array 65536))
         (idx 0)
         (pos 0))
    (dotimes (i 65536) (setf (aref table i) i))
    (loop while (< pos n)
          do (let ((u (%fat-le bytes (* 2 pos) 2)))
               ;; 0xFFFF followed by a count is a run; 0xFFFF as the last unit is the
               ;; mapping of U+FFFF itself
               (cond ((and (= u #xFFFF) (< (+ pos 1) n))
                      (setq idx (+ idx (%fat-le bytes (* 2 (+ pos 1)) 2)))
                      (setq pos (+ pos 2)))
                     (t
                      (when (>= idx 65536)
                        (error "fat32: up-case table has more than 65536 entries"))
                      (setf (aref table idx) u)
                      (setq idx (+ idx 1))
                      (setq pos (+ pos 1))))))
    (unless (= idx 65536)
      (error "fat32: up-case table covers ~a units, not 65536" idx))
    table))

(defvar *fat-upcase* nil "The default up-case table, decoded once on first use.")

(defun %fat-upcase-unit (c)
  "The up-case of the UTF-16 unit C by the default table."
  (unless *fat-upcase*
    (setq *fat-upcase* (%upcase-decode (%hex-bytes (%default-upcase-hex)))))
  (if (< c 65536) (aref *fat-upcase* c) c))

(defun %fat-fold (ch)
  "The up-case of CH as a code, for case-insensitive comparison."
  (%fat-upcase-unit (char-code ch)))

(defun %fat-name= (a b)
  (and (= (length a) (length b))
       (let ((i 0) (n (length a)) (same t))
         (loop
           (when (>= i n) (return))
           (unless (= (%fat-fold (char a i)) (%fat-fold (char b i)))
             (setq same nil)
             (return))
           (setq i (+ i 1)))
         same)))

(defun %fat-name-chars (raw off len lower)
  "Chars of the LEN-byte 8.3 field at OFF, trailing spaces dropped.  LOWER folds
   A-Z to a-z (the NT case flags)."
  (let ((n len))
    (loop
      (when (or (= n 0) (/= (aref raw (+ off n -1)) 32)) (return))
      (setq n (- n 1)))
    (let ((acc nil) (i 0))
      (loop
        (when (>= i n) (return))
        (let ((b (aref raw (+ off i))))
          (when (and lower (>= b 65) (<= b 90)) (setq b (+ b 32)))
          (setq acc (cons (code-char b) acc)))
        (setq i (+ i 1)))
      (nreverse acc))))

(defun %fat-short-name (raw off)
  "The 8.3 name of the directory entry at OFF, with the NT case flags applied."
  (let* ((flags (aref raw (+ off 12)))
         (base (%fat-name-chars raw off 8 (/= 0 (logand flags 8))))
         (ext (%fat-name-chars raw (+ off 8) 3 (/= 0 (logand flags 16)))))
    ;; A first byte of 0x05 stands for a real 0xE5 (a deleted-marker clash).
    (when (= (aref raw off) 5) (setf (car base) (code-char 229)))
    (%fat-string (if ext (append base (cons #\. ext)) base))))

(defun %utf16-encode (name)
  "NAME as a list of UTF-16 code units.  A character above U+FFFF becomes a surrogate
   pair, so a name's length in units can differ from its length in characters."
  (let ((out nil))
    (loop for ch across name
          do (let ((c (char-code ch)))
               (if (< c #x10000)
                   (push c out)
                   (let ((x (- c #x10000)))
                     (push (+ #xD800 (ash x -10)) out)
                     (push (+ #xDC00 (logand x #x3FF)) out)))))
    (nreverse out)))

(defun %utf16-decode (units)
  "The string of the UTF-16 code UNITS.  A surrogate pair becomes one character; a
   lone surrogate is kept as a character of its own."
  (let ((out nil))
    (loop while units
          do (let ((u (pop units)))
               (if (and (<= #xD800 u #xDBFF) units (<= #xDC00 (car units) #xDFFF))
                   (let ((lo (pop units)))
                     (push (code-char (+ #x10000 (ash (- u #xD800) 10) (- lo #xDC00))) out))
                   (push (code-char u) out))))
    (%fat-string (nreverse out))))

(defun %fat-lfn-chars (raw off)
  "The UTF-16 code units of the VFAT long-name entry at OFF, as a list, up to the NUL."
  (let ((acc nil) (done nil))
    (dolist (o '(1 3 5 7 9 14 16 18 20 22 24 28 30))
      (unless done
        (let ((u (+ (aref raw (+ off o)) (* 256 (aref raw (+ off o 1))))))
          (cond ((= u 0) (setq done t))
                ((= u #xFFFF) nil)
                (t (setq acc (cons u acc)))))))
    (nreverse acc)))

(defun %fat-lfn-join (pieces)
  "The UTF-16 code units of the LFN PIECES, each (SEQ . UNITS), in ascending SEQ order."
  (let ((acc nil))
    (dolist (p pieces)
      (setq acc (append acc (cdr p))))
    acc))

(defun %fat-sfn-checksum (raw off)
  "The checksum of the 11-byte 8.3 name at OFF, as the VFAT LFN entries store it."
  (let ((sum 0))
    (dotimes (i 11)
      (setq sum (logand (+ (ash (logand sum 1) 7) (ash sum -1) (aref raw (+ off i))) 255)))
    sum))

;;; ---- directories ----------------------------------------------------------------

(defun %fat-scan-dir (v cluster)
  "The live entries of the directory whose chain starts at CLUSTER, in disk order,
   with their slots.  Returns (values ENTRIES RAW CLUSTERS).  Each entry is
   (NAME DIRP SIZE FIRST-CLUSTER FIRST-SLOT SLOT SFN): FIRST-SLOT through SLOT are
   the slots of its long-name run and short entry, and SFN is its 11-byte short
   name.  Slot numbers index 32-byte entries in RAW, which is the directory's
   bytes in chain order.  \".\" and \"..\" are skipped."
  (let* ((clusters (%fat-chain v cluster))
         (raw (%fat-read-clusters v clusters (* (length clusters) (%fat-cluster-bytes v))))
         (nslots (floor (length raw) 32))
         (out nil)
         (lfn nil)          ; LFN pieces of the run being read
         (lfn-sum 0)        ; checksum carried by that run's first (highest) piece
         (lfn-first nil)    ; slot of that run's first piece
         )
    (dotimes (i nslots)
      (let* ((off (* 32 i))
             (b0 (aref raw off))
             (attr (aref raw (+ off 11))))
        (cond
          ((= b0 0) (return))                          ; end of directory
          ((= b0 #xE5) (setq lfn nil lfn-first nil))   ; deleted entry
          ((= (logand attr #x3F) #x0F)                 ; VFAT long-name piece
           (when (/= 0 (logand b0 #x40))
             (setq lfn nil)
             (setq lfn-sum (aref raw (+ off 13)))
             (setq lfn-first i))
           (setq lfn (cons (cons (logand b0 #x1F) (%fat-lfn-chars raw off)) lfn)))
          ((/= 0 (logand attr #x08))                   ; volume label
           (setq lfn nil lfn-first nil))
          ((= b0 #x2E)                                 ; "." and ".."
           (setq lfn nil lfn-first nil))
          (t
           (let* ((short (%fat-short-name raw off))
                  (use-lfn (and lfn (= lfn-sum (%fat-sfn-checksum raw off))))
                  (name (if use-lfn (%utf16-decode (%fat-lfn-join lfn)) short))
                  (dirp (/= 0 (logand attr #x10)))
                  (cluster (+ (* 65536 (%fat-le raw (+ off 20) 2)) (%fat-le raw (+ off 26) 2)))
                  (size (%fat-le raw (+ off 28) 4)))
             (push (list name dirp (if dirp 0 size) cluster
                         (if use-lfn lfn-first i) i (subseq raw off (+ off 11)))
                   out)
             (setq lfn nil lfn-first nil))))))
    (values (nreverse out) raw clusters)))

(defun %fat-dir-entries (v cluster)
  "Entries of the directory at CLUSTER as (NAME DIRP SIZE FIRST-CLUSTER)."
  (let ((entries (%fat-scan-dir v cluster)))
    (mapcar (lambda (e) (list (nth 0 e) (nth 1 e) (nth 2 e) (nth 3 e))) entries)))

(defun %fat-find (entries name)
  (let ((hit nil))
    (dolist (e entries)
      (when (and (null hit) (%fat-name= (nth 0 e) name))
        (setq hit e)))
    hit))

(defun %fat-split (path)
  "The non-empty '/'-separated components of PATH."
  (let ((parts nil) (cur nil))
    (dotimes (i (length path))
      (let ((ch (char path i)))
        (cond ((char= ch #\/)
               (when cur (setq parts (cons (%fat-string (reverse cur)) parts)))
               (setq cur nil))
              (t (setq cur (cons ch cur))))))
    (when cur (setq parts (cons (%fat-string (reverse cur)) parts)))
    (nreverse parts)))

(defun %fat-walk (v cluster parts)
  "The entry named by PARTS below the directory at CLUSTER, or NIL."
  (let ((hit (%fat-find (%fat-dir-entries v cluster) (car parts))))
    (cond ((null hit) nil)
          ((null (cdr parts)) hit)
          ((nth 1 hit) (%fat-walk v (nth 3 hit) (cdr parts)))
          (t nil))))

;;; ---- public API -----------------------------------------------------------------

(defun fat32-lookup (v path)
  (let ((parts (%fat-split path)))
    (if (null parts)
        (list "" t 0 (%fat-root v))
        (%fat-walk v (%fat-root v) parts))))

(defun fat32-directory (v path)
  (let ((e (fat32-lookup v path)))
    (when (and e (nth 1 e))
      (%fat-dir-entries v (nth 3 e)))))

(defun fat32-read-file (v path)
  (let ((e (fat32-lookup v path)))
    (when (and e (not (nth 1 e)))
      (%fat-read-clusters v (%fat-chain v (nth 3 e)) (nth 2 e)))))

;;; ---- devices ------------------------------------------------------------------------
;;; A device is a simulated disk.  It has two images.  The VIEW is what the running
;;; system reads, and every write lands in it at once.  The DURABLE image is what a
;;; crash would leave: it changes only at a flush.  Writes are kept in order, so a
;;; crash can keep any subset of the writes since the last flush, as a disk may
;;; reorder them.  A device can also be told to die at event N, where events are
;;; the writes and flushes numbered from 0.  After it dies, nothing it is given
;;; persists, so the code that runs on afterwards cannot change what a crash leaves.
;;;
;;; A device is the vector #(VIEW FLUSHED PENDING EVENTS CRASH-AT DEAD WRITE-FN
;;; FLUSH-FN):
;;;   FLUSHED and PENDING hold (OFFSET . BYTES) entries, newest first.
;;;   WRITE-FN, when set, is called as (WRITE-FN OFFSET BYTES) for each write, so a
;;;   real image follows the view; FLUSH-FN, when set, is called with no arguments at
;;;   each flush and must make the writes so far durable.  Both are NIL for a memory
;;;   device.
;;;
;;; A BLOCK device has no view: a disk too big to hold in memory, such as an SD card.
;;; Its slots are #(NIL NIL NIL EVENTS CRASH-AT DEAD WRITE-FN FLUSH-FN READ-FN SIZE).
;;; Reads go to READ-FN, called as (READ-FN OFFSET N) for a fresh vector of N bytes;
;;; writes go to WRITE-FN alone, and nothing is logged, so it has no crash simulation.
;;; A write lies within one sector but may cover only part of it: a disk that writes
;;; whole sectors must read the rest of the sector and write it back.

(defun make-fat32-device (view &optional write-fn flush-fn)
  "A device whose view is VIEW, which it modifies in place."
  (vector view nil nil 0 nil nil write-fn flush-fn nil nil))

(defun make-fat32-block-device (size read-fn &optional write-fn flush-fn)
  "A device of SIZE bytes read through READ-FN and written through WRITE-FN, with no copy
   of its contents in memory.  Without WRITE-FN it is read-only: a write signals."
  (vector nil nil nil 0 nil nil write-fn flush-fn read-fn size))

(defun %device-block-p (dev) (null (svref dev 0)))

(defun device-size (dev)
  "The device's size in bytes."
  (if (%device-block-p dev) (svref dev 9) (length (svref dev 0))))

(defun fat32-open-device (dev)
  (fat32-open (if (%device-block-p dev) (device-read dev 0 512) (svref dev 0)) dev))

(defun device-read (dev off n)
  "N bytes at OFF of the view, as a fresh vector."
  (when (or (< off 0) (> (+ off n) (device-size dev)))
    (error "fat32: read past end of volume at byte ~a" (+ off n)))
  (if (%device-block-p dev)
      (funcall (svref dev 8) off n)
      (subseq (svref dev 0) off (+ off n))))

(defun device-dead-p (dev) (svref dev 5))

(defun device-events (dev) (svref dev 3))

(defun device-write (dev off src)
  "A sector write: it reaches the view now, and the durable image at the next flush."
  (unless (svref dev 5)
    (when (eql (svref dev 3) (svref dev 4))
      (setf (svref dev 5) t)
      (return-from device-write nil))
    (setf (svref dev 3) (+ (svref dev 3) 1))
    (cond ((%device-block-p dev)
           (unless (svref dev 6) (error "fat32: the device is read-only"))
           (funcall (svref dev 6) off src))
          (t
           (push (cons off (copy-seq src)) (svref dev 2))
           (%fat-copy-into (svref dev 0) off src)
           (when (svref dev 6)
             (funcall (svref dev 6) off src))))))

(defun device-flush (dev)
  "Everything written so far becomes durable."
  (unless (svref dev 5)
    (when (eql (svref dev 3) (svref dev 4))
      (setf (svref dev 5) t)
      (return-from device-flush nil))
    (setf (svref dev 3) (+ (svref dev 3) 1))
    (unless (%device-block-p dev)
      (setf (svref dev 1) (append (svref dev 2) (svref dev 1)))
      (setf (svref dev 2) nil))
    (when (svref dev 7)
      (funcall (svref dev 7)))))

(defun device-crash-writes (dev keep)
  "The writes a crash now leaves, oldest first: everything flushed, and of the
   unflushed writes those KEEP selects.  KEEP is :ALL, :NONE, or a function of an
   unflushed write's position (oldest is 0)."
  (let ((out (reverse (svref dev 1)))
        (i 0))
    (dolist (w (reverse (svref dev 2)))
      (when (cond ((eq keep :all) t)
                  ((eq keep :none) nil)
                  (t (funcall keep i)))
        (setq out (append out (list w))))
      (setq i (+ i 1)))
    out))

;;; ---- writing ------------------------------------------------------------------------
;;; Writes change the byte vector given to FAT32-OPEN, in place.  The caller keeps the
;;; bytes and saves them.  Nothing is cached: each call reads the state it needs.
;;;
;;; Crash safety.  Every change goes through %FAT-WRITE, one sector write.  A disk
;;; writes a sector whole or not at all, so no write is torn.  An operation is a
;;; sequence of such writes, and %FAT-BARRIER marks where everything before it must
;;; be durable before anything after it may land.  The rules that keep a crash
;;; harmless:
;;;
;;;   - data and FAT links are durable before the directory entry that refers to
;;;     them;
;;;   - a new name's long-name entries are durable before its short entry, which is
;;;     the commit point of a create;
;;;   - a deleted entry is durable before its clusters are freed, and an entry that
;;;     moves is durable at its new place before its old place goes;
;;;   - a directory's new cluster is zeroed and linked before anything is written
;;;     into it.
;;;
;;; A crash can then leave leaked clusters (fsck.fat -a reclaims them), a rename
;;; visible under both names, or none of the operation at all.  It does not leave an
;;; entry pointing at clusters that are not allocated to it.  A caller that needs an
;;; operation to have happened must end with (FAT32-SYNC V).
;;;
;;; Writes reach a device when the volume has one; see the devices section.  The
;;; sector writes are what the device sees, and the barriers are its flushes.

(defun %fat-put (bytes off n value)
  "Store VALUE in N little-endian bytes at OFF."
  (let ((x value))
    (dotimes (i n)
      (when (or (< (+ off i) 0) (>= (+ off i) (length bytes)))
        (error "fat32: write past end of volume at byte ~a" (+ off i)))
      (setf (aref bytes (+ off i)) (logand x 255))
      (setq x (ash x -8)))))

(defun %fat-barrier (v)
  (let ((dev (%fat-device v)))
    (when dev
      (device-flush dev))))

(defun fat32-sync (v)
  "Everything written so far is durable before anything written after."
  (%fat-barrier v))

(defun %fat-write (v off src)
  "Write the bytes SRC at OFF as one sector write.  SRC must lie within one sector."
  (let* ((bps (%fat-bps v))
         (n (length src)))
    (when (and (> n 0) (/= (floor off bps) (floor (+ off n -1) bps)))
      (error "fat32: internal: ~a bytes at ~a cross a sector boundary" n off))
    (let ((dev (%fat-device v)))
      (if dev
          (device-write dev off src)
          (%fat-copy-into (%fat-bytes v) off src)))))

(defun %fat-copy-into (bytes off src)
  (dotimes (i (length src))
    (%fat-put bytes (+ off i) 1 (aref src i))))

(defun %fat-le-bytes (value n)
  "VALUE as N little-endian bytes, in a fresh vector."
  (let ((e (make-array n)))
    (dotimes (i n) (setf (aref e i) (logand (ash value (* -8 i)) 255)))
    e))

(defun %fat-write-fill (v off n value)
  "N copies of the byte VALUE at OFF, in sector-sized writes."
  (let* ((bps (%fat-bps v))
         (pos off)
         (end (+ off n)))
    (loop while (< pos end)
          do (let* ((room (- (* bps (+ 1 (floor pos bps))) pos))
                    (chunk (min room (- end pos)))
                    (buf (make-array chunk)))
               (dotimes (i chunk) (setf (aref buf i) value))
               (%fat-write v pos buf)
               (setq pos (+ pos chunk))))))

(defun %fat-set-next (v c value)
  "Write FAT entry C to VALUE in every FAT copy, keeping the reserved top nibble.
   Each copy is its own sector write, in copy order."
  (dotimes (i (%fat-nfats v))
    (let* ((off (+ (%fat-fat-start v) (* i (%fat-fatsz v)) (* c 4)))
           (old (%fat-read-le v off 4)))
      (%fat-write v off (%fat-le-bytes (logior (logand old #xF0000000)
                                               (logand value #x0FFFFFFF))
                                       4)))))

;;; FSInfo (FAT32 only): the free-cluster count and the next-free hint.  Both are
;;; advisory, and 0xFFFFFFFF means unknown.  Keeping them right keeps fsck quiet.

(defun %fat-adjust-free (v delta)
  (let ((fi (%fat-fsinfo v)))
    (when fi
      (let ((free (%fat-read-le v (+ fi #x1E8) 4)))
        (unless (= free #xFFFFFFFF)
          (%fat-write v (+ fi #x1E8) (%fat-le-bytes (+ free delta) 4)))))))

(defun %fat-hint-get (v)
  "Where the allocator starts looking: the FSInfo hint if it is in range, else 2."
  (let ((fi (%fat-fsinfo v)))
    (if fi
        (let ((h (%fat-read-le v (+ fi #x1EC) 4)))
          (if (and (>= h 2) (<= h (+ (%fat-nclusters v) 1))) h 2))
        2)))

(defun %fat-hint-set (v c)
  (let ((fi (%fat-fsinfo v)))
    (when fi
      (%fat-write v (+ fi #x1EC) (%fat-le-bytes c 4)))))

(defun %fat-alloc (v)
  "Claim a free cluster: mark it end-of-chain and return its number."
  (let* ((n (%fat-nclusters v))
         (start (%fat-hint-get v)))
    (dotimes (k n (error "fat32: volume is full"))
      (let ((c (+ 2 (mod (+ (- start 2) k) n))))
        (when (= (%fat-next v c) 0)
          (%fat-set-next v c #x0FFFFFFF)
          (%fat-adjust-free v -1)
          (%fat-hint-set v (if (>= (+ c 1) (+ n 2)) 2 (+ c 1)))
          (return-from %fat-alloc c))))))

(defun %fat-alloc-chain (v n)
  "N clusters linked in order, as a list.  If the volume runs out part way, the
   clusters already claimed are released before the error."
  (let ((got nil) (ok nil))
    (unwind-protect
         (progn
           (dotimes (i n)
             (let ((c (%fat-alloc v)))
               (when got (%fat-set-next v (car got) c))
               (push c got)))
           (setq ok t)
           (reverse got))
      (unless ok
        (dolist (c got)
          (%fat-set-next v c 0)
          (%fat-adjust-free v 1))))))

(defun %fat-free-clusters (v clusters)
  (dolist (c clusters)
    (%fat-set-next v c 0)
    (%fat-adjust-free v 1)))

(defun %fat-free-chain (v first)
  "Free the chain that starts at FIRST."
  (%fat-free-clusters v (%fat-chain v first)))

(defun %fat-zero-cluster (v c)
  (%fat-write-fill v (%fat-cluster-offset v c) (%fat-cluster-bytes v) 0))

(defun %fat-store (v clusters data)
  "Write DATA across CLUSTERS in order, one sector write at a time, zero-filling
   the tail of the last cluster."
  (let* ((cs (%fat-cluster-bytes v))
         (bps (%fat-bps v))
         (n (length data))
         (pos 0))
    (dolist (c clusters)
      (let ((off (%fat-cluster-offset v c)))
        (loop for k from 0 below cs by bps
              do (let ((buf (make-array bps)))
                   (dotimes (i bps)
                     (setf (aref buf i) (if (< pos n) (aref data pos) 0))
                     (setq pos (+ pos 1)))
                   (%fat-write v (+ off k) buf)))))))

;;; ---- directory slots ----------------------------------------------------------------

(defun %fat-slot-offset (v clusters i)
  "Byte offset of directory slot I (32 bytes) in the chain CLUSTERS."
  (let* ((cs (%fat-cluster-bytes v))
         (pos (* 32 i)))
    (+ (%fat-cluster-offset v (nth (floor pos cs) clusters)) (mod pos cs))))

(defun %fat-put-slot (v clusters i entry)
  (%fat-write v (%fat-slot-offset v clusters i) entry))

(defun %fat-free-run (raw k)
  "First slot index I such that slots I .. I+K-1 are all free in RAW, or NIL."
  (let ((run 0) (start 0))
    (dotimes (i (floor (length raw) 32) nil)
      (let ((b0 (aref raw (* 32 i))))
        (if (or (= b0 0) (= b0 #xE5))
            (progn
              (when (= run 0) (setq start i))
              (setq run (+ run 1))
              (when (>= run k) (return start)))
            (setq run 0))))))

(defun %fat-extend-dir (v clusters)
  "Give a directory one more zeroed cluster at its end.  Returns the new chain."
  (let ((c (car (%fat-alloc-chain v 1))))
    (%fat-zero-cluster v c)
    (%fat-barrier v)                    ; zeroed before it is linked
    (%fat-set-next v (car (last clusters)) c)
    (%fat-barrier v)                    ; linked before anything is written into it
    (append clusters (list c))))

;;; ---- names ---------------------------------------------------------------------------

(defun %fat-check-name (name)
  (let ((n (length name)))
    (when (or (= n 0) (> (length (%utf16-encode name)) 255)
              (string= name ".") (string= name "..")
              (char= (char name (- n 1)) #\Space) (char= (char name (- n 1)) #\.)
              (find-if (lambda (ch)
                         (or (char= ch #\/) (< (char-code ch) 32)
                             (find ch "\\:*?\"<>|")))
                       name))
      (error "fat32: invalid name ~s" name))))

(defun %fat-sfn-char-p (ch)
  "A character a short name may hold, once upcased."
  (or (and (char>= ch #\A) (char<= ch #\Z))
      (and (char>= ch #\0) (char<= ch #\9))
      (find ch "!#$%&'()-@^_`{}~")))

(defun %fat-split-ext (name)
  "BASE and EXT of NAME, split at its last dot.  EXT is NIL when there is none."
  (let ((dot (position #\. name :from-end t)))
    (if (and dot (> dot 0))
        (values (subseq name 0 dot) (subseq name (+ dot 1)))
        (values name nil))))

(defun %fat-exact-field (s max allow-empty)
  "S as an 8.3 field, upcased, as (values UPCASED LOWER-P); NIL if it cannot be
   stored exactly.  Mixed case is not exact: the NT flags cover only all-lower."
  (when (and (or allow-empty (> (length s) 0)) (<= (length s) max))
    (let ((lower nil) (upper nil) (ok t))
      (loop for ch across s
            do (cond ((and (char>= ch #\a) (char<= ch #\z)) (setq lower t))
                     ((and (char>= ch #\A) (char<= ch #\Z)) (setq upper t))
                     ((%fat-sfn-char-p ch) nil)
                     (t (setq ok nil))))
      (when (and ok (not (and lower upper)))
        (values (string-upcase s) lower)))))

(defun %fat-make-sfn (base ext)
  (let ((sfn (make-array 11)))
    (dotimes (i 11) (setf (aref sfn i) 32))
    (dotimes (i (length base)) (setf (aref sfn i) (char-code (char base i))))
    (dotimes (i (length ext)) (setf (aref sfn (+ 8 i)) (char-code (char ext i))))
    sfn))

(defun %fat-clean (s)
  "S upcased, with spaces and dots dropped and other unusable characters as _."
  (let ((out nil))
    (loop for ch across (string-upcase s)
          do (cond ((or (char= ch #\Space) (char= ch #\.)) nil)
                   ((%fat-sfn-char-p ch) (push ch out))
                   (t (push #\_ out))))
    (coerce (nreverse out) 'string)))

(defun %fat-generated-sfn (base ext taken)
  "A short name for a long name: the cleaned stem with a ~N tail, unique among TAKEN."
  (let* ((stem (%fat-clean base))
         (xt (if ext (subseq (%fat-clean ext) 0 (min 3 (length (%fat-clean ext)))) ""))
         (stem (if (= (length stem) 0) "_" stem)))
    (loop for n from 1
          do (let* ((tail (format nil "~~~a" n))
                    (keep (min (length stem) (- 8 (length tail))))
                    (cand (%fat-make-sfn (concatenate 'string (subseq stem 0 keep) tail) xt)))
               (unless (member cand taken :test #'equalp)
                 (return cand))))))

(defun %fat-name-plan (name taken)
  "How NAME is stored, given the short names already in use (TAKEN):
   (values SFN FLAGS LONG-P).  SFN is the 11-byte short name, FLAGS the NT case
   byte, and LONG-P is T when long-name entries are needed as well."
  (multiple-value-bind (base ext) (%fat-split-ext name)
    (multiple-value-bind (ub lb) (%fat-exact-field base 8 nil)
      (multiple-value-bind (ue le) (if ext (%fat-exact-field ext 3 t) (values "" nil))
        (if (and ub ue)
            (values (%fat-make-sfn ub ue) (logior (if lb 8 0) (if le 16 0)) nil)
            (values (%fat-generated-sfn base ext taken) 0 t))))))

(defun %fat-entry-bytes (sfn attr flags cluster size)
  "A 32-byte short directory entry."
  (let ((e (make-array 32))
        (date (logior (ash 46 9) (ash 10 5) 8)))   ; 2026-10-08, a fixed stamp
    (dotimes (i 32) (setf (aref e i) 0))
    (dotimes (i 11) (setf (aref e i) (aref sfn i)))
    (setf (aref e 11) attr)
    (setf (aref e 12) flags)
    (%fat-put e 16 2 date)
    (%fat-put e 18 2 date)
    (%fat-put e 20 2 (ash cluster -16))
    (%fat-put e 24 2 date)
    (%fat-put e 26 2 (logand cluster #xFFFF))
    (%fat-put e 28 4 size)
    e))

(defun %fat-lfn-entry (units seq last-p sum)
  "The long-name entry for sequence number SEQ.  UNITS is the whole name as UCS-2
   code units, terminated and padded to a multiple of 13."
  (let ((e (make-array 32))
        (offsets '(1 3 5 7 9 14 16 18 20 22 24 28 30)))
    (dotimes (i 32) (setf (aref e i) 0))
    (setf (aref e 0) (logior seq (if last-p #x40 0)))
    (setf (aref e 11) #x0F)
    (setf (aref e 13) sum)
    (loop for o in offsets
          for k from (* 13 (- seq 1))
          do (%fat-put e o 2 (aref units k)))
    e))

(defun %fat-units (name)
  "NAME as UTF-16 units: the units, a NUL, then 0xFFFF padding to 13 each."
  (let* ((codes (%utf16-encode name))
         (n (length codes))
         (nlfn (ceiling (+ n 1) 13))
         (units (make-array (* 13 nlfn))))
    (dotimes (i (length units)) (setf (aref units i) #xFFFF))
    (loop for c in codes for i from 0 do (setf (aref units i) c))
    (setf (aref units n) 0)
    units))

;;; ---- entries ------------------------------------------------------------------------

(defun %fat-add-entry (v dir-cluster name dirp first size)
  "Write the entries for NAME (its long-name run and short entry) into the directory
   at DIR-CLUSTER, growing the directory if it is full.  The short entry is written
   last, after a barrier, so a crash cannot leave a short entry without its names."
  (multiple-value-bind (entries raw clusters) (%fat-scan-dir v dir-cluster)
    (multiple-value-bind (sfn flags long-p)
        (%fat-name-plan name (mapcar (lambda (e) (nth 6 e)) entries))
      (let* ((nlfn (if long-p (ceiling (+ (length (%utf16-encode name)) 1) 13) 0))
             (k (+ nlfn 1))
             (start (%fat-free-run raw k)))
        (loop until start
              do (setq clusters (%fat-extend-dir v clusters))
                 (setq raw (%fat-read-clusters v clusters
                                               (* (length clusters) (%fat-cluster-bytes v))))
                 (setq start (%fat-free-run raw k)))
        (when long-p
          (let ((units (%fat-units name))
                (sum (%fat-sfn-checksum sfn 0)))
            (loop for seq from nlfn downto 1
                  do (%fat-put-slot v clusters (+ start (- nlfn seq))
                                    (%fat-lfn-entry units seq (= seq nlfn) sum)))
            (%fat-barrier v)))
        (%fat-put-slot v clusters (+ start nlfn)
                       (%fat-entry-bytes sfn (if dirp #x10 #x20) flags first size))))))

(defun %fat-resolve-parent (v path)
  "(values PARENT-CLUSTER BASE-NAME) for PATH.  The parent directory must exist."
  (let ((parts (%fat-split path)))
    (when (null parts)
      (error "fat32: no name in ~s" path))
    (let* ((base (car (last parts)))
           (dirs (butlast parts))
           (e (if dirs (%fat-walk v (%fat-root v) dirs) (list "" t 0 (%fat-root v)))))
      (unless (and e (nth 1 e))
        (error "fat32: parent directory is missing for ~s" path))
      (values (nth 3 e) base))))

(defun %fat-set-dotdot (v dir target)
  "Point the \"..\" entry of the directory at DIR (its second slot) at TARGET.  One
   sector write, so the entry is never half updated."
  (let* ((off (+ (%fat-cluster-offset v dir) 32))
         (e (%fat-read v off 32)))
    (%fat-put e 20 2 (ash target -16))
    (%fat-put e 26 2 (logand target #xFFFF))
    (%fat-write v off e)))

(defun %fat-mark-deleted (v clusters first last)
  "Mark directory slots FIRST through LAST deleted: one byte each, E5."
  (let ((gone (make-array 1)))
    (setf (aref gone 0) #xE5)
    (loop for i from first to last
          do (%fat-write v (%fat-slot-offset v clusters i) gone))))

(defun fat32-write-file (v path bytes)
  "Create the file at PATH holding BYTES, or replace the contents of the file there.
   The new data is durable before the entry points at it; the old chain is freed
   only after the entry no longer points at it."
  (multiple-value-bind (parent base) (%fat-resolve-parent v path)
    (%fat-check-name base)
    (multiple-value-bind (entries raw clusters) (%fat-scan-dir v parent)
      (declare (ignore raw))
      (let* ((old (%fat-find entries base))
             (cs (%fat-cluster-bytes v))
             (n (length bytes))
             (chain nil)
             (done nil))
        (when (and old (nth 1 old))
          (error "fat32: ~s is a directory" path))
        (setq chain (%fat-alloc-chain v (ceiling n cs)))
        (unwind-protect
             (progn
               (%fat-store v chain bytes)
               (%fat-barrier v)                 ; data and FAT links before the entry
               (if old
                   (let* ((off (%fat-slot-offset v clusters (nth 5 old)))
                          (e (%fat-read v off 32))
                          (head (if chain (car chain) 0)))
                     (%fat-put e 20 2 (ash head -16))
                     (%fat-put e 26 2 (logand head #xFFFF))
                     (%fat-put e 28 4 n)
                     (%fat-write v off e))
                   (%fat-add-entry v parent base nil (if chain (car chain) 0) n))
               (setq done t))
          (unless done
            (%fat-free-clusters v chain)))
        (when old
          (%fat-barrier v)                      ; the new entry before the old chain goes
          (%fat-free-chain v (nth 3 old)))
        nil))))

(defun fat32-mkdir (v path)
  "Create an empty directory at PATH, with its \".\" and \"..\" entries."
  (multiple-value-bind (parent base) (%fat-resolve-parent v path)
    (%fat-check-name base)
    (multiple-value-bind (entries) (%fat-scan-dir v parent)
      (when (%fat-find entries base)
        (error "fat32: ~s already exists" path)))
    (let* ((c (car (%fat-alloc-chain v 1)))
           (done nil))
      (unwind-protect
           (progn
             (%fat-zero-cluster v c)
             (let ((dot (make-array 11))
                   (dotdot (make-array 11))
                   (off (%fat-cluster-offset v c)))
               (dotimes (i 11)
                 (setf (aref dot i) 32)
                 (setf (aref dotdot i) 32))
               (setf (aref dot 0) 46)
               (setf (aref dotdot 0) 46 (aref dotdot 1) 46)
               (%fat-write v off (%fat-entry-bytes dot #x10 0 c 0))
               (%fat-write v (+ off 32)
                           (%fat-entry-bytes dotdot #x10 0
                                             (if (= parent (%fat-root v)) 0 parent) 0)))
             (%fat-barrier v)                  ; the directory complete before its entry
             (%fat-add-entry v parent base t c 0)
             (setq done t))
        (unless done
          (%fat-free-clusters v (list c)))))
    nil))

(defun fat32-delete (v path)
  "Remove the file at PATH, or the empty directory there.  The entry is deleted
   before its clusters are freed."
  (multiple-value-bind (parent base) (%fat-resolve-parent v path)
    (multiple-value-bind (entries raw clusters) (%fat-scan-dir v parent)
      (declare (ignore raw))
      (let ((e (%fat-find entries base)))
        (unless e
          (error "fat32: no such entry ~s" path))
        (when (and (nth 1 e) (%fat-scan-dir v (nth 3 e)))
          (error "fat32: directory ~s is not empty" path))
        (%fat-mark-deleted v clusters (nth 4 e) (nth 5 e))
        (%fat-barrier v)
        (%fat-free-chain v (nth 3 e))
        nil))))

(defun fat32-rename (v old new)
  "Give the entry at OLD the name NEW.  NEW may be in another directory, which must
   exist.  The entry keeps its data.  The old name is removed before the new one is
   added, so the two entries never share a cluster chain on disk: a shared chain is
   a cross-link, which fsck.fat repairs by dropping one of them.  A crash between
   the two leaves the data unreferenced, and fsck.fat -a recovers it as a FILE*.CHK
   file in the root.  If the new name cannot be added, the old entry is restored.
   A directory moved to another parent gets its \"..\" updated, and moving a
   directory into itself is refused."
  (multiple-value-bind (oparent obase) (%fat-resolve-parent v old)
    (multiple-value-bind (oentries oraw oclusters) (%fat-scan-dir v oparent)
      (declare (ignore oraw))
      (let ((e (%fat-find oentries obase)))
        (unless e
          (error "fat32: no such entry ~s" old))
        (when (and (nth 1 e)
                   (> (length new) (length old))
                   (string= old new :end2 (length old))
                   (char= (char new (length old)) #\/))
          (error "fat32: cannot move ~s into itself" old))
        (multiple-value-bind (nparent nbase) (%fat-resolve-parent v new)
          (%fat-check-name nbase)
          (let ((clash (%fat-find (%fat-scan-dir v nparent) nbase)))
            (when (and clash
                       (not (and (= nparent oparent) (= (nth 5 clash) (nth 5 e)))))
              (error "fat32: ~s already exists" new)))
          (let* ((first-slot (nth 4 e))
                 (last-slot (nth 5 e))
                 (saved (loop for i from first-slot to last-slot
                              collect (let ((off (%fat-slot-offset v oclusters i)))
                                        (%fat-read v off 32))))
                 (done nil))
            (%fat-mark-deleted v oclusters first-slot last-slot)
            (%fat-barrier v)                    ; the old name is gone before the new one
            (unwind-protect
                 (progn
                   (%fat-add-entry v nparent nbase (nth 1 e) (nth 3 e) (nth 2 e))
                   (%fat-barrier v)
                   (when (and (nth 1 e) (/= nparent oparent))
                     (%fat-set-dotdot v (nth 3 e) (if (= nparent (%fat-root v)) 0 nparent))
                     (%fat-barrier v))
                   (setq done t))
              (unless done
                (loop for i from first-slot
                      for s in saved
                      do (%fat-write v (%fat-slot-offset v oclusters i) s)))))
          nil)))))

;;; ---- formatting ----------------------------------------------------------------------
;;; FAT32-FORMAT makes an empty FAT32 volume on a device: what mkfs.vfat -F 32 writes, by
;;; Microsoft's FAT specification (cluster size by volume size, the FAT32 FAT-size formula,
;;; FSInfo and a backup boot sector at 6).  Every sector the volume's structures cover is
;;; written -- the reserved area, both FATs, the root cluster -- since a card's old contents
;;; are not zeros.  The boot sector goes LAST, after a flush: a format cut off part way leaves
;;; no signature, so nothing mistakes the half-written area for a volume.

(defun %fat32-cluster-sectors (total-sectors)
  "Sectors per cluster for a volume of TOTAL-SECTORS 512-byte sectors (the specification's
   table: 4 KB clusters to 8 GB, then 8, 16, 32 KB)."
  (cond ((<= total-sectors 532480) 1)          ; 260 MB
        ((<= total-sectors 16777216) 8)        ; 8 GB
        ((<= total-sectors 33554432) 16)       ; 16 GB
        ((<= total-sectors 67108864) 32)       ; 32 GB
        (t 64)))

(defun fat32-format (dev &key (label "NO NAME") sectors-per-cluster (volume-id 0) (hidden-sectors 0))
  "Make an empty FAT32 volume of the whole of DEV (512-byte sectors).  LABEL is up to 11
   characters; HIDDEN-SECTORS is the partition's first sector on its disk (the BPB records it).
   Returns the open volume."
  (let* ((bps 512)
         (total (floor (device-size dev) bps))
         (spc (or sectors-per-cluster (%fat32-cluster-sectors total)))
         (rsvd 32) (nfats 2)
         ;; the specification's FAT32 sizing: FAT sectors enough for the clusters left after them
         (fatsz (ceiling (- total rsvd) (floor (+ (* 256 spc) nfats) 2)))
         (data-start (+ rsvd (* nfats fatsz)))
         (nclusters (floor (- total data-start) spc))
         (zero (make-array bps :initial-element 0)))
    (unless (and (> spc 0) (<= spc 128) (= 0 (logand spc (- spc 1))))
      (error "fat32-format: sectors per cluster ~a is not a power of two up to 128" spc))
    (when (< nclusters 65525)
      (error "fat32-format: ~a sectors make ~a clusters of ~a sectors; FAT32 needs at least 65525"
             total nclusters spc))
    (flet ((put (sector bytes) (device-write dev (* sector bps) bytes))
           (le (bytes off n value) (%fat-put bytes off n value))
           (ascii (bytes off string n)
             (dotimes (i n)
               (setf (aref bytes (+ off i))
                     (if (< i (length string)) (char-code (char-upcase (char string i))) 32)))))
      ;; the reserved area, both FATs and the root cluster: zeros
      (dotimes (s rsvd) (put s zero))
      (dotimes (s (* nfats fatsz)) (put (+ rsvd s) zero))
      (dotimes (s spc) (put (+ data-start s) zero))
      ;; FAT[0] the media byte, FAT[1] end of chain, FAT[2] the root directory's one cluster
      (let ((f (make-array bps :initial-element 0)))
        (le f 0 4 #x0FFFFFF8) (le f 4 4 #x0FFFFFFF) (le f 8 4 #x0FFFFFFF)
        (dotimes (i nfats) (put (+ rsvd (* i fatsz)) f)))
      ;; the volume label as the root directory's first entry
      (when (and label (string/= label "NO NAME"))
        (let ((d (make-array bps :initial-element 0)))
          (ascii d 0 label 11)
          (setf (aref d 11) #x08)
          (put data-start d)))
      ;; FSInfo, and its backup at 7: every cluster but the root's free, the next from 3
      (let ((fi (make-array bps :initial-element 0)))
        (le fi 0 4 #x41615252) (le fi 484 4 #x61417272)
        (le fi 488 4 (- nclusters 1)) (le fi 492 4 3)
        (le fi 508 4 #xAA550000)
        (put 1 fi) (put 7 fi))
      (device-flush dev)
      ;; the boot sector: its backup at 6 first, then sector 0, the signature last of all
      (let ((b (make-array bps :initial-element 0)))
        (setf (aref b 0) #xEB (aref b 1) #x58 (aref b 2) #x90)
        (ascii b 3 "MODUS" 8)
        (le b 11 2 bps) (setf (aref b 13) spc) (le b 14 2 rsvd) (setf (aref b 16) nfats)
        (setf (aref b 21) #xF8)                     ; media: fixed disk
        (le b 24 2 63) (le b 26 2 255)              ; sectors per track, heads (nominal)
        (le b 28 4 hidden-sectors) (le b 32 4 total)
        (le b 36 4 fatsz) (le b 44 4 2)             ; FAT size; root at cluster 2
        (le b 48 2 1) (le b 50 2 6)                 ; FSInfo at 1, backup boot sector at 6
        (setf (aref b 64) #x80 (aref b 66) #x29)    ; drive number; extended boot signature
        (le b 67 4 volume-id)
        (ascii b 71 (or label "NO NAME") 11)
        (ascii b 82 "FAT32" 8)
        (setf (aref b 510) #x55 (aref b 511) #xAA)
        (put 6 b)
        (device-flush dev)
        (put 0 b))
      (device-flush dev)
      (fat32-open-device dev))))
