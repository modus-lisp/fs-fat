;;;; lib/exfat.lisp -- exFAT reader and writer (pure CL, no FFI).
;;;;
;;;; Requires lib/fat32.lisp to be loaded first: the device layer (make-fat32-device,
;;;; device-read, device-write, device-flush) and the path helpers (%fat-split,
;;;; %fat-find, %fat-u8, %fat-le) are shared.
;;;;
;;;; Reads and writes an exFAT volume held in a byte vector, or through a device, as
;;;; fat32.lisp does.  Scope:
;;;;   - one FAT (NumberOfFats 1); TexFAT volumes are refused;
;;;;   - files and directories are read by FAT chain or contiguously (NoFatChain),
;;;;     and always written as FAT chains;
;;;;   - names are written as ASCII (the up-case table for ASCII is the default one),
;;;;     and read as UTF-16, compared case-insensitively for ASCII letters;
;;;;   - entry sets are written within one sector, so each commit is one sector write.
;;;;
;;;; Public API:
;;;;   (exfat-open bytes &optional device)  -> volume.  An ERROR if not exFAT.
;;;;   (exfat-lookup v path)                -> entry or NIL.
;;;;   (exfat-directory v path)             -> entries of a directory, or NIL.
;;;;   (exfat-read-file v path)             -> fresh byte vector, or NIL.
;;;;   (exfat-write-file v path bytes)      -> create or replace a file.
;;;;   (exfat-mkdir v path)                 -> create an empty directory.
;;;;   (exfat-delete v path)                -> remove a file or an empty directory.
;;;;   (exfat-rename v old new)             -> move or rename an entry.
;;;;   (exfat-sync v)                       -> everything so far becomes durable.
;;;;
;;;; An entry is (NAME DIRP SIZE FIRST-CLUSTER).
;;;;
;;;; Crash ordering follows fat32.lisp: data and FAT entries and bitmap bits before
;;;; the entry set that refers to them; a deleted set before its clusters are freed.
;;;; The entry set is the commit point, and it is one sector write.

;;; ---- volume ----------------------------------------------------------------------
;;; A volume is the list (BYTES DEVICE BPS SPC FAT-BYTE DATA-BYTE ROOT NCLUSTERS
;;; BITMAP-CLUSTERS BITMAP-BYTES UPCASE).  UPCASE is the volume's up-case table: a
;;; vector of 65536 units, entry U the up-case of unit U.

(in-package #:fs-fat)

;; Used before their definitions below, so a source LOAD does not warn about them.
(declaim (ftype function %ex-free-clusters %ex-set-dir-size %ex-lookup-raw))

(defun %ex-bytes (v) (nth 0 v))
(defun %ex-dev (v) (nth 1 v))
(defun %ex-bps (v) (nth 2 v))
(defun %ex-spc (v) (nth 3 v))
(defun %ex-fat (v) (nth 4 v))
(defun %ex-data (v) (nth 5 v))
(defun %ex-root (v) (nth 6 v))
(defun %ex-nclusters (v) (nth 7 v))
(defun %ex-bitmap (v) (nth 8 v))
(defun %ex-bitmap-bytes (v) (nth 9 v))
(defun %ex-upcase-table (v) (nth 10 v))

(defun %ex-upcase (v c)
  "The up-case of the UTF-16 unit C, by the volume's table."
  (if (< c 65536) (aref (%ex-upcase-table v) c) c))

(defun %ex-cs (v) (* (%ex-bps v) (%ex-spc v)))

(defun %ex-cluster-off (v c)
  (+ (%ex-data v) (* (- c 2) (%ex-cs v))))

(defun %ex-chain-offset (v clusters pos)
  "Byte offset of byte POS of the data whose chain is CLUSTERS."
  (let ((cs (%ex-cs v)))
    (+ (%ex-cluster-off v (nth (floor pos cs) clusters)) (mod pos cs))))

(defun %ex-slot-offset (v clusters i)
  "Byte offset of directory slot I (a 32-byte entry) in the chain CLUSTERS."
  (%ex-chain-offset v clusters (* 32 i)))

(defun %ex-read (v off n)
  "N bytes at OFF, as a fresh vector, through the device when there is one."
  (let ((dev (%ex-dev v)))
    (if dev
        (device-read dev off n)
        (subseq (%ex-bytes v) off (+ off n)))))

(defun %ex-write (v off src)
  "Write SRC at OFF as one sector write.  SRC must lie within one sector."
  (let* ((bps (%ex-bps v))
         (n (length src)))
    (when (and (> n 0) (/= (floor off bps) (floor (+ off n -1) bps)))
      (error "exfat: internal: ~a bytes at ~a cross a sector boundary" n off))
    (let ((dev (%ex-dev v)))
      (if dev
          (device-write dev off src)
          (%fat-copy-into (%ex-bytes v) off src)))))

(defun %ex-barrier (v)
  (let ((dev (%ex-dev v)))
    (when dev
      (device-flush dev))))

(defun exfat-sync (v)
  "Everything written so far is durable before anything written after."
  (%ex-barrier v))

(defun %ex-fat-get (v c)
  (%fat-le (%ex-read v (+ (%ex-fat v) (* 4 c)) 4) 0 4))

(defun %ex-fat-set (v c value)
  (%ex-write v (+ (%ex-fat v) (* 4 c)) (%fat-le-bytes value 4)))

;;; ---- chains and data -------------------------------------------------------------

(defun %ex-chain (v first contiguous size)
  "The clusters of the chain starting at FIRST: contiguous when CONTIGUOUS (SIZE says
   how many), otherwise followed through the FAT.  FIRST below 2 is an empty chain."
  (let ((n (%ex-nclusters v)))
    (cond ((< first 2) nil)
          (contiguous
           (let ((count (ceiling size (%ex-cs v))))
             (when (> (+ first count -1) (+ n 1))
               (error "exfat: contiguous chain runs past the last cluster"))
             (loop for i from 0 below count collect (+ first i))))
          (t
           (let ((acc nil) (count 0) (c first))
             (loop
               (when (or (< c 2) (> c (+ n 1)))
                 (error "exfat: cluster ~a is outside the data area" c))
               (when (>= count n)
                 (error "exfat: cluster chain is longer than the volume"))
               (push c acc)
               (setq count (+ count 1))
               (setq c (%ex-fat-get v c))
               (when (>= c #xFFFFFFF8) (return)))
             (nreverse acc))))))

(defun %ex-read-clusters (v clusters size)
  "Fresh vector of the first SIZE bytes of CLUSTERS.  SIZE is checked against the
   chain's capacity before anything is allocated."
  (let* ((cs (%ex-cs v))
         (capacity (* (length clusters) cs)))
    (when (> size capacity)
      (error "exfat: chain holds ~a bytes, ~a expected" capacity size))
    (let ((out (make-array size))
          (pos 0))
      (dolist (c clusters)
        (let ((data (%ex-read v (%ex-cluster-off v c) cs)))
          (dotimes (i cs)
            (when (>= pos size) (return))
            (setf (aref out pos) (aref data i))
            (setq pos (+ pos 1)))))
      out)))

(defun %ex-store (v clusters data)
  "Write DATA across CLUSTERS in order, one sector at a time, zero-filling the tail."
  (let* ((cs (%ex-cs v))
         (bps (%ex-bps v))
         (n (length data))
         (pos 0))
    (dolist (c clusters)
      (let ((off (%ex-cluster-off v c)))
        (loop for k from 0 below cs by bps
              do (let ((buf (make-array bps)))
                   (dotimes (i bps)
                     (setf (aref buf i) (if (< pos n) (aref data pos) 0))
                     (setq pos (+ pos 1)))
                   (%ex-write v (+ off k) buf)))))))

(defun %ex-zero-cluster (v c)
  (let ((off (%ex-cluster-off v c))
        (bps (%ex-bps v)))
    (loop for k from 0 below (%ex-cs v) by bps
          do (let ((buf (make-array bps)))
               (dotimes (i bps) (setf (aref buf i) 0))
               (%ex-write v (+ off k) buf)))))

;;; ---- the allocation bitmap ----------------------------------------------------------

(defun %ex-bitmap-set (v c value)
  "Mark cluster C allocated (VALUE 1) or free (VALUE 0) in the bitmap: one byte write."
  (let* ((i (- c 2))
         (pos (floor i 8))
         (bit (ash 1 (mod i 8)))
         (off (%ex-chain-offset v (%ex-bitmap v) pos))
         (old (aref (%ex-read v off 1) 0))
         (new (if (= value 1) (logior old bit) (logand old (lognot bit) #xFF)))
         (buf (make-array 1)))
    (setf (aref buf 0) new)
    (%ex-write v off buf)))

(defun %ex-alloc (v)
  "Claim a free cluster: its bitmap bit and its FAT entry (end of chain).  Returns
   the cluster number."
  (let* ((n (%ex-nclusters v))
         (bm (%ex-read-clusters v (%ex-bitmap v) (%ex-bitmap-bytes v))))
    (dotimes (i n (error "exfat: volume is full"))
      (when (= 0 (logand (aref bm (floor i 8)) (ash 1 (mod i 8))))
        (let ((c (+ i 2)))
          (%ex-bitmap-set v c 1)
          (%ex-fat-set v c #xFFFFFFFF)
          (return-from %ex-alloc c))))))

(defun %ex-alloc-chain (v n)
  "N clusters linked in order.  If the volume runs out, what was claimed is released."
  (let ((got nil) (ok nil))
    (unwind-protect
         (progn
           (dotimes (i n)
             (let ((c (%ex-alloc v)))
               (when got (%ex-fat-set v (car got) c))
               (push c got)))
           (setq ok t)
           (reverse got))
      (unless ok
        (%ex-free-clusters v got)))))

(defun %ex-free-clusters (v clusters)
  (dolist (c clusters)
    (%ex-bitmap-set v c 0)
    (%ex-fat-set v c 0)))

;;; ---- directories ---------------------------------------------------------------------

(defun %ex-u16 (raw off)
  (%fat-le raw off 2))

(defun %ex-set-checksum (e n)
  "The 16-bit checksum of the N bytes of entry set E, skipping its checksum field."
  (let ((sum 0))
    (dotimes (i n)
      (unless (or (= i 2) (= i 3))
        (setq sum (logand (+ (logior (ash sum -1) (ash sum 15)) (aref e i)) #xFFFF))))
    sum))

(defun %ex-name-hash (v name)
  "The name hash of NAME, computed over its up-cased UTF-16 units."
  (let ((h 0))
    (dolist (c (%utf16-encode name))
      (let ((u (%ex-upcase v c)))
        (dolist (b (list (logand u 255) (ash u -8)))
          (setq h (logand (+ (logior (ash h -1) (ash h 15)) b) #xFFFF)))))
    h))

(defun %ex-name= (v a b)
  "Whether the names A and B are equal once up-cased by the volume's table."
  (let ((ua (%utf16-encode a)) (ub (%utf16-encode b)))
    (and (= (length ua) (length ub))
         (every (lambda (x y) (= (%ex-upcase v x) (%ex-upcase v y))) ua ub))))

(defun %ex-find (v entries name)
  "The first of ENTRIES whose name matches NAME case-insensitively, by the up-case table."
  (find-if (lambda (e) (%ex-name= v (nth 0 e) name)) entries))

(defun %ex-set-bytes (v name dirp first size contiguous)
  "The entry set for NAME: the file entry, the stream extension, and the name entries.
   Returns a fresh vector of 32 * (2 + name entries) bytes."
  (let* ((units (%utf16-encode name))
         (nunits (length units))
         (nname (max 1 (ceiling nunits 15)))
         (total (+ 2 nname))
         (e (make-array (* 32 total)))
         (date (logior (ash 46 9) (ash 10 5) 8)))   ; 2026-10-08, a fixed stamp
    (dotimes (i (length e)) (setf (aref e i) 0))
    ;; file directory entry
    (setf (aref e 0) #x85)
    (setf (aref e 1) (+ nname 1))
    (%fat-put e 4 2 (if dirp #x10 #x20))
    (%fat-put e 10 2 date)
    (%fat-put e 14 2 date)
    (%fat-put e 18 2 date)
    ;; stream extension
    (let ((s 32))
      (setf (aref e s) #xC0)
      (setf (aref e (+ s 1)) (logior 1 (if contiguous 2 0)))
      (setf (aref e (+ s 3)) nunits)
      (%fat-put e (+ s 4) 2 (%ex-name-hash v name))
      (%fat-put e (+ s 8) 8 size)
      (%fat-put e (+ s 20) 4 first)
      (%fat-put e (+ s 24) 8 size))
    ;; name entries, fifteen UTF-16 units each
    (dotimes (k nname)
      (let ((s (* 32 (+ 2 k))))
        (setf (aref e s) #xC1)
        (dotimes (j 15)
          (let ((i (+ (* 15 k) j)))
            (when (< i nunits)
              (%fat-put e (+ s 2 (* 2 j)) 2 (nth i units)))))))
    (%fat-put e 2 2 (%ex-set-checksum e (length e)))
    e))

(defun %ex-scan-dir (v first contiguous size)
  "The live entry sets of the directory with chain FIRST, in disk order, with their
   slots.  Returns (values ENTRIES RAW CLUSTERS).  Each entry is
   (NAME DIRP SIZE FIRST-CLUSTER CONTIGUOUS FIRST-SLOT COUNT).  Entries whose
   checksum does not match are skipped, as are entries that are not in use."
  (let* ((clusters (%ex-chain v first contiguous size))
         (raw (%ex-read-clusters v clusters (* (length clusters) (%ex-cs v))))
         (nslots (floor (length raw) 32))
         (out nil)
         (i 0))
    (loop while (< i nslots)
          do (let* ((off (* 32 i))
                    (b0 (aref raw off)))
               (cond
                 ((= b0 0) (return))                       ; end of directory
                 ((and (= b0 #x85) (>= (aref raw (+ off 1)) 2)
                       (<= (+ i (aref raw (+ off 1)) 1) nslots))
                  (let* ((n (aref raw (+ off 1)))
                         (set-bytes (subseq raw off (+ off (* 32 (+ n 1)))))
                         (s (+ off 32)))
                    (if (and (= (aref raw s) #xC0)
                             (>= (- n 1) (ceiling (aref raw (+ s 3)) 15))
                             (= (%ex-u16 raw (+ off 2)) (%ex-set-checksum set-bytes (length set-bytes)))
                             (loop for k from 0 below (- n 1)
                                   always (= (aref raw (+ s (* 32 (+ k 1)))) #xC1)))
                        (let* ((namelen (aref raw (+ s 3)))
                               (units (loop for k from 0 below namelen
                                            collect (let ((u (+ (+ s 32) (* 32 (floor k 15))
                                                                2 (* 2 (mod k 15)))))
                                                      (%ex-u16 raw u))))
                               (name (%utf16-decode units))
                               (dirp (/= 0 (logand (%ex-u16 raw (+ off 4)) #x10)))
                               (first (%fat-le raw (+ s 20) 4))
                               (contig (/= 0 (logand (aref raw (+ s 1)) 2)))
                               (size (%fat-le raw (+ s 24) 8)))
                          (push (list name dirp size first contig i (+ n 1)) out)
                          (setq i (+ i n 1)))
                        (setq i (+ i 1)))))
                 (t (setq i (+ i 1))))))
    (values (nreverse out) raw clusters)))

(defun %ex-free-run (raw total bps)
  "First slot I such that slots I .. I+TOTAL-1 are free and lie within one sector."
  (let ((nslots (floor (length raw) 32)))
    (dotimes (s (+ 1 (- nslots total)) nil)
      (when (and (loop for j from 0 below total
                       never (/= 0 (logand (aref raw (* 32 (+ s j))) #x80)))
                 (= (floor (* 32 s) bps)
                    (floor (+ (* 32 s) (* 32 total) -1) bps)))
        (return s)))))

(defun %ex-extend-dir (v contiguous clusters)
  "Give a directory one more zeroed cluster.  Returns the new chain."
  (when contiguous
    (error "exfat: a contiguous directory cannot grow"))
  (let ((c (car (%ex-alloc-chain v 1))))
    (%ex-zero-cluster v c)
    (%ex-barrier v)                       ; zeroed before it is linked
    (%ex-fat-set v (car (last clusters)) c)
    (%ex-barrier v)                       ; linked before anything is written into it
    (append clusters (list c))))

(defun %ex-first-end (raw)
  "The first slot whose type is 0x00, the end of the directory, or the slot count."
  (let ((nslots (floor (length raw) 32)))
    (dotimes (i nslots nslots)
      (when (= (aref raw (* 32 i)) 0)
        (return i)))))

(defun %ex-add-set (v pfirst pcont psize pentry set-bytes)
  "Write SET-BYTES into a free run of the directory, growing the directory if it is
   full.  The set is one sector write, so it appears whole or not at all.
   The reader stops at the first end-of-directory entry, so a set placed after one is
   preceded by in-use markers (type 0x01, which is not in use): the end marker is
   never left in front of a live entry."
  (let ((total (floor (length set-bytes) 32)))
    (multiple-value-bind (entries raw clusters) (%ex-scan-dir v pfirst pcont psize)
      (declare (ignore entries))
      (let ((start (%ex-free-run raw total (%ex-bps v))))
        (loop until start
              do (setq clusters (%ex-extend-dir v pcont clusters))
                 (when pentry
                   (%ex-barrier v)
                   (%ex-set-dir-size v pentry (* (length clusters) (%ex-cs v))))
                 (setq raw (%ex-read-clusters v clusters (* (length clusters) (%ex-cs v))))
                 (setq start (%ex-free-run raw total (%ex-bps v))))
        (let ((end (%ex-first-end raw)))
          (loop for i from end below start
                do (let ((buf (make-array 1)))
                     (setf (aref buf 0) 1)
                     (%ex-write v (%ex-slot-offset v clusters i) buf))))
        (%ex-write v (%ex-slot-offset v clusters start) set-bytes)
        start))))

(defun %ex-mark-deleted (v clusters first count)
  "Clear the in-use bit of COUNT entries from slot FIRST: one byte write each."
  (loop for i from first below (+ first count)
        do (let* ((off (%ex-slot-offset v clusters i))
                  (b (aref (%ex-read v off 1) 0))
                  (buf (make-array 1)))
             (setf (aref buf 0) (logand b #x7F))
             (%ex-write v off buf))))

;;; ---- names and paths ----------------------------------------------------------------

(defun %ex-check-name (name)
  "A name for a new entry.  Its length is in UTF-16 units, at most 210: 14 name
   entries, so the whole set is one sector write."
  (let ((n (length name)))
    (when (or (= n 0) (> (length (%utf16-encode name)) 210)
              (string= name ".") (string= name "..")
              (char= (char name (- n 1)) #\Space) (char= (char name (- n 1)) #\.)
              (find-if (lambda (ch)
                         (or (char= ch #\/) (< (char-code ch) 32)
                             (find ch "\\:*?\"<>|")))
                       name))
      (error "exfat: invalid name ~s" name))))

(defun %ex-walk (v first contiguous size parts)
  "The entry for PARTS below the directory with chain FIRST, or NIL."
  (let* ((entries (%ex-scan-dir v first contiguous size))
         (hit (%ex-find v entries (car parts))))
    (cond ((null hit) nil)
          ((null (cdr parts)) hit)
          ((nth 1 hit) (%ex-walk v (nth 3 hit) (nth 4 hit) (nth 2 hit) (cdr parts)))
          (t nil))))

(defun %ex-resolve-parent (v path)
  "(values PFIRST PCONT PSIZE BASE PENTRY) for PATH.  The parent directory must exist.
   PENTRY locates the parent's own entry set, as (CONTAINING-FIRST CONTAINING-CONT
   CONTAINING-SIZE SLOT), so that its size can be kept current as it grows; it is NIL
   for the root, which has no entry."
  (let ((parts (%fat-split path)))
    (when (null parts)
      (error "exfat: no name in ~s" path))
    (let ((first (%ex-root v)) (cont nil) (size 0) (pentry nil))
      (dolist (d (butlast parts))
        (let* ((entries (%ex-scan-dir v first cont size))
               (e (%ex-find v entries d)))
          (unless (and e (nth 1 e))
            (error "exfat: parent directory is missing for ~s" path))
          (setq pentry (list first cont size (nth 5 e)))
          (setq first (nth 3 e) cont (nth 4 e) size (nth 2 e))))
      (values first cont size (car (last parts)) pentry))))

(defun %ex-set-dir-size (v pentry size)
  "Set the size of a directory, whose entry set is located by PENTRY, to SIZE: the
   stream extension's DataLength and ValidDataLength, with the set checksum redone,
   as one sector write."
  (destructuring-bind (cfirst ccont csize slot) pentry
    (let* ((clusters (%ex-chain v cfirst ccont csize))
           (off (%ex-slot-offset v clusters slot))
           (count (+ 1 (aref (%ex-read v (+ off 1) 1) 0)))
           (set (%ex-read v off (* 32 count))))
      (%fat-put set (+ 32 8) 8 size)
      (%fat-put set (+ 32 24) 8 size)
      (%fat-put set 2 2 (%ex-set-checksum set (length set)))
      (%ex-write v off set))))

(defun %ex-entry-clusters (v e)
  "The clusters of the entry E, as found in its directory listing."
  (%ex-chain v (nth 3 e) (nth 4 e) (nth 2 e)))

;;; ---- open and read ----------------------------------------------------------------------

(defun %ex-find-bitmap (raw)
  "(FIRST CONTIGUOUS SIZE) of the allocation bitmap entry in the root directory."
  (loop for i from 0 below (floor (length raw) 32)
        do (let ((off (* 32 i)))
             (when (= (aref raw off) #x81)
               (return-from %ex-find-bitmap
                 (list (%fat-le raw (+ off 20) 4)
                       (/= 0 (logand (aref raw (+ off 1)) 2))
                       (%fat-le raw (+ off 24) 8))))
             (when (= (aref raw off) 0)
               (return))))
  (error "exfat: no allocation bitmap in the root directory"))

(defun %ex-table-checksum (bytes)
  "The checksum of an up-case table, as its directory entry records it."
  (let ((c 0))
    (dotimes (k (length bytes) c)
      (setq c (logand (+ (logior (ash c -1) (ash c 31)) (aref bytes k)) #xFFFFFFFF)))))

(defun %ex-load-upcase (v raw)
  "The volume's up-case table, from the up-case entry (type 0x82) in the root RAW.
   Its checksum must match the entry's."
  (let ((off (loop for i from 0 below (floor (length raw) 32)
                   for o = (* 32 i)
                   until (= (aref raw o) 0)
                   when (= (aref raw o) #x82) return o)))
    (unless off
      (error "exfat: no up-case table in the root directory"))
    (let* ((first (%fat-le raw (+ off 20) 4))
           (len (%fat-le raw (+ off 24) 8))
           (sum (%fat-le raw (+ off 4) 4))
           (bytes (%ex-read-clusters v (%ex-chain v first nil len) len)))
      (unless (= (%ex-table-checksum bytes) sum)
        (error "exfat: up-case table checksum is ~8,'0x, expected ~8,'0x"
               (%ex-table-checksum bytes) sum))
      (%upcase-decode bytes))))

(defun exfat-open (bytes &optional device)
  "A volume over BYTES.  DEVICE, when given, is the disk BYTES is the view of."
  (let ((len (length bytes)))
    (when (< len 512)
      (error "exfat: volume is ~a bytes, too short for a boot sector" len))
    (unless (and (= (%fat-u8 bytes 510) #x55) (= (%fat-u8 bytes 511) #xAA))
      (error "exfat: no boot sector signature"))
    (unless (and (= (%fat-u8 bytes 3) 69) (= (%fat-u8 bytes 4) 88)
                 (= (%fat-u8 bytes 5) 70) (= (%fat-u8 bytes 6) 65)
                 (= (%fat-u8 bytes 7) 84) (= (%fat-u8 bytes 8) 32))
      (error "exfat: not an exFAT volume"))
    (let* ((bps-shift (%fat-u8 bytes 108))
           (spc-shift (%fat-u8 bytes 109))
           (nfats (%fat-u8 bytes 110))
           (fat-sector (%fat-le bytes 80 4))
           (fat-len (%fat-le bytes 84 4))
           (heap-sector (%fat-le bytes 88 4))
           (ccount (%fat-le bytes 92 4))
           (root (%fat-le bytes 96 4))
           (vol-sectors (%fat-le bytes 72 8)))
      (unless (<= 9 bps-shift 12)
        (error "exfat: bytes-per-sector shift ~a is not 9..12" bps-shift))
      (unless (<= 0 spc-shift 25)
        (error "exfat: sectors-per-cluster shift ~a is out of range" spc-shift))
      (unless (= nfats 1)
        (error "exfat: ~a FATs (TexFAT) are not supported" nfats))
      (let* ((bps (ash 1 bps-shift))
             (spc (ash 1 spc-shift))
             (cs (* bps spc)))
        (unless (<= cs (* 32 1024 1024))
          (error "exfat: cluster size ~a is over 32 MiB" cs))
        (unless (and (>= fat-sector 1) (>= fat-len 1)
                     (>= (* fat-len bps) (* 4 (+ ccount 2))))
          (error "exfat: the FAT is too small for ~a clusters" ccount))
        (unless (<= (+ heap-sector (* ccount spc)) vol-sectors)
          (error "exfat: the cluster heap runs past the end of the volume"))
        (unless (<= (* vol-sectors bps) len)
          (error "exfat: volume declares ~a bytes but only ~a are present"
                 (* vol-sectors bps) len))
        (unless (<= 2 root (+ ccount 1))
          (error "exfat: root cluster ~a is invalid" root))
        (let* ((v0 (list bytes device bps spc (* fat-sector bps) (* heap-sector bps)
                         root ccount nil 0 nil))
               (root-chain (%ex-chain v0 root nil 0))
               (raw (%ex-read-clusters v0 root-chain (* (length root-chain) cs)))
               (bm (%ex-find-bitmap raw)))
          (setf (nth 8 v0) (%ex-chain v0 (first bm) (second bm) (third bm)))
          (setf (nth 9 v0) (third bm))
          (setf (nth 10 v0) (%ex-load-upcase v0 raw))
          v0)))))

(defun exfat-lookup (v path)
  (let ((parts (%fat-split path)))
    (if (null parts)
        (list "" t 0 (%ex-root v))
        (let ((e (%ex-walk v (%ex-root v) nil 0 parts)))
          (and e (list (nth 0 e) (nth 1 e) (nth 2 e) (nth 3 e)))))))

(defun exfat-directory (v path)
  (let ((e (%ex-lookup-raw v path)))
    (when (and e (nth 1 e))
      (mapcar (lambda (x) (list (nth 0 x) (nth 1 x) (nth 2 x) (nth 3 x)))
              (%ex-scan-dir v (nth 3 e) (nth 4 e) (nth 2 e))))))

(defun %ex-lookup-raw (v path)
  "The full internal entry for PATH, or NIL.  The root is (\"\" T 0 ROOT NIL NIL 0)."
  (let ((parts (%fat-split path)))
    (if (null parts)
        (list "" t 0 (%ex-root v) nil 0 0)
        (%ex-walk v (%ex-root v) nil 0 parts))))

(defun exfat-read-file (v path)
  (let ((e (%ex-lookup-raw v path)))
    (when (and e (not (nth 1 e)))
      (%ex-read-clusters v (%ex-chain v (nth 3 e) (nth 4 e) (nth 2 e)) (nth 2 e)))))

;;; ---- writing ---------------------------------------------------------------------------
;;; Writes change the volume in place, through its device.  The ordering is the same as
;;; in fat32.lisp: data, FAT entries and bitmap bits first, then the entry set that
;;; refers to them as one sector write, then, if an old chain is being replaced, the
;;; barrier, then the old chain's release.

(defun exfat-write-file (v path bytes)
  "Create the file at PATH holding BYTES, or replace the contents of the file there."
  (multiple-value-bind (pfirst pcont psize base pentry) (%ex-resolve-parent v path)
    (%ex-check-name base)
    (multiple-value-bind (entries raw clusters) (%ex-scan-dir v pfirst pcont psize)
      (declare (ignore raw))
      (let* ((old (%ex-find v entries base))
             (n (length bytes))
             (chain nil)
             (done nil))
        (when (and old (nth 1 old))
          (error "exfat: ~s is a directory" path))
        (setq chain (%ex-alloc-chain v (ceiling n (%ex-cs v))))
        (unwind-protect
             (progn
               (%ex-store v chain bytes)
               (%ex-barrier v)
               (let ((first (if chain (car chain) 0)))
                 (if old
                     (%ex-write v (%ex-slot-offset v clusters (nth 5 old))
                                (%ex-set-bytes v base nil first n nil))
                     (%ex-add-set v pfirst pcont psize pentry
                                  (%ex-set-bytes v base nil first n nil))))
               (setq done t))
          (unless done
            (%ex-free-clusters v chain)))
        (when old
          (%ex-barrier v)
          (%ex-free-clusters v (%ex-entry-clusters v old)))
        nil))))

(defun exfat-mkdir (v path)
  "Create an empty directory at PATH."
  (multiple-value-bind (pfirst pcont psize base pentry) (%ex-resolve-parent v path)
    (%ex-check-name base)
    (multiple-value-bind (entries) (%ex-scan-dir v pfirst pcont psize)
      (when (%ex-find v entries base)
        (error "exfat: ~s already exists" path)))
    (let ((c (car (%ex-alloc-chain v 1)))
          (done nil))
      (unwind-protect
           (progn
             (%ex-zero-cluster v c)
             (%ex-barrier v)                 ; the directory is empty before its entry
             (%ex-add-set v pfirst pcont psize pentry
                          (%ex-set-bytes v base t c (%ex-cs v) nil))
             (setq done t))
        (unless done
          (%ex-free-clusters v (list c)))))
    nil))

(defun exfat-delete (v path)
  "Remove the file at PATH, or the empty directory there.  The entry is marked deleted
   before its clusters are freed."
  (multiple-value-bind (pfirst pcont psize base) (%ex-resolve-parent v path)
    (multiple-value-bind (entries raw clusters) (%ex-scan-dir v pfirst pcont psize)
      (declare (ignore raw))
      (let ((e (%ex-find v entries base)))
        (unless e
          (error "exfat: no such entry ~s" path))
        (when (and (nth 1 e) (%ex-scan-dir v (nth 3 e) (nth 4 e) (nth 2 e)))
          (error "exfat: directory ~s is not empty" path))
        (%ex-mark-deleted v clusters (nth 5 e) 1)
        (%ex-barrier v)
        (%ex-free-clusters v (%ex-entry-clusters v e))
        (%ex-mark-deleted v clusters (+ 1 (nth 5 e)) (- (nth 6 e) 1))
        nil))))

(defun exfat-rename (v old new)
  "Give the entry at OLD the name NEW, which may be in another directory.  The old
   set is marked deleted before the new one is written, so the two never coexist on
   disk.  If the new set cannot be written, the old one is restored."
  (multiple-value-bind (opfirst opcont opsize obase) (%ex-resolve-parent v old)
    (multiple-value-bind (oentries oraw oclusters) (%ex-scan-dir v opfirst opcont opsize)
      (declare (ignore oraw))
      (let ((e (%ex-find v oentries obase)))
        (unless e
          (error "exfat: no such entry ~s" old))
        (when (and (nth 1 e)
                   (> (length new) (length old))
                   (string= old new :end2 (length old))
                   (char= (char new (length old)) #\/))
          (error "exfat: cannot move ~s into itself" old))
        (multiple-value-bind (npfirst npcont npsize nbase npentry) (%ex-resolve-parent v new)
          (%ex-check-name nbase)
          (let ((clash (%ex-find v (%ex-scan-dir v npfirst npcont npsize) nbase)))
            (when (and clash
                       (not (and (= npfirst opfirst) (= (nth 5 clash) (nth 5 e)))))
              (error "exfat: ~s already exists" new)))
          (let* ((first-slot (nth 5 e))
                 (count (nth 6 e))
                 (saved (loop for i from first-slot below (+ first-slot count)
                              collect (%ex-read v (%ex-slot-offset v oclusters i) 32)))
                 (set-bytes (%ex-set-bytes v nbase (nth 1 e) (nth 3 e) (nth 2 e) (nth 4 e)))
                 (done nil))
            (%ex-mark-deleted v oclusters first-slot 1)
            (%ex-barrier v)
            (unwind-protect
                 (progn
                   (%ex-add-set v npfirst npcont npsize npentry set-bytes)
                   (setq done t))
              (unless done
                (loop for i from first-slot
                      for s in saved
                      do (%ex-write v (%ex-slot-offset v oclusters i) s))))
            (%ex-mark-deleted v oclusters (+ first-slot 1) (- count 1))))))
    nil))
