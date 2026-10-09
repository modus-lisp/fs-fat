;;;; test/fat32-oracle.lisp -- mkfs.vfat, mtools and fsck.fat as the oracle for
;;;; lib/fat32.lisp, plus a patch fuzzer.  Portable CL except HOST-RUN.
;;;;
;;;; test/run-fat32-oracle.sh defines these before loading this file:
;;;;   *oracle-root*        repo root, with a trailing slash
;;;;   *oracle-workdir*     a fresh empty directory for this run
;;;;   *oracle-first-seed*  the first seed
;;;;   *oracle-seeds*       how many consecutive seeds to run
;;;;   *oracle-cases*       fuzz cases per seed
;;;;
;;;; Per seed:
;;;;  1. Build a random host tree in memory and write it out: nested directories,
;;;;     mixed-case and long names, empty and multi-cluster files, one directory
;;;;     holding a few hundred entries.  mkfs.vfat -F 32 an image, copy the tree
;;;;     in with mtools, delete one or two files from the image and from the
;;;;     host.  The in-memory model is the ground truth.
;;;;  2. fsck.fat -n must accept the image.
;;;;  3. PRIMARY ORACLE: the reader's walk of the unpatched image equals the
;;;;     model -- same paths, kinds, sizes and FNV-1a digests.
;;;;  4. FUZZ: each case is a few byte patches applied in memory (the image is
;;;;     never copied), judged by category:
;;;;       data    one byte of file data: status ok, and only the owner changes
;;;;       size    the owner's directory size field: if the new size fits its
;;;;               cluster chain, ok and only the owner changes; if not,
;;;;               fat32-error
;;;;       fat     one FAT entry:         ok, fat32-error or limit
;;;;       dirent  bytes of a directory entry: ok, fat32-error or limit
;;;;       bpb     boot-sector bytes:     ok, fat32-error or limit
;;;;     Under every category, status "internal" is a failure.
;;;;
;;;; PORTABILITY: everything is portable CL except HOST-RUN, the one function
;;;; that spawns a process.  It uses sb-ext:run-program for now.

(load (concatenate 'string *oracle-root* "lib/fat32.lisp"))
(use-package :fs-fat)
(load (concatenate 'string *oracle-root* "test/fat32-walk.lisp"))

(defvar *mkfs* "/sbin/mkfs.vfat")
(defvar *fsck* "/sbin/fsck.fat")
(defvar *mcopy* "mcopy")
(defvar *mdel* "mdel")
(defvar *mkdir* "mkdir")
(defvar *rm* "rm")

;;; ---- the host seam ------------------------------------------------------------

(defun host-run (program &rest args)
  "Run PROGRAM with ARGS, no shell.  Returns (values exit-code output).
   The only host-only function in this file: process creation is not portable
   CL.  Under SBCL it is sb-ext:run-program."
  (let* ((out (make-string-output-stream))
         (p (sb-ext:run-program program args :output out :error out :search t
                                :environment (cons "MTOOLS_SKIP_CHECK=1"
                                                   (sb-ext:posix-environ)))))
    (values (sb-ext:process-exit-code p) (get-output-stream-string out))))

(defun host-must (program &rest args)
  "Run PROGRAM; signal an error unless it exits 0.  Returns its output."
  (multiple-value-bind (code out) (apply #'host-run program args)
    (unless (= code 0)
      (error "~a failed (exit ~a): ~a" program code out))
    out))

;;; ---- randomness: xorshift32, so a seed reproduces the run exactly ------------

(defun rng-make (seed)
  (list (let ((s (logand seed #xFFFFFFFF))) (if (zerop s) 1 s))))

(defun rng-next (r)
  (let ((x (car r)))
    (setq x (logand (logxor x (ash x 13)) #xFFFFFFFF))
    (setq x (logxor x (ash x -17)))
    (setq x (logand (logxor x (ash x 5)) #xFFFFFFFF))
    (setf (car r) x)))

(defun rng-int (r n) (mod (rng-next r) n))
(defun rng-range (r lo hi) (+ lo (rng-int r (+ 1 (- hi lo)))))
(defun rng-real (r) (/ (rng-next r) 4294967296.0))
(defun rng-chance (r p) (< (rng-real r) p))
(defun rng-pick (r items) (nth (rng-int r (length items)) items))

(defun rand-byte (r)
  (if (rng-chance r 0.25)
      (rng-pick r '(0 229 15 16 32 255))
      (rng-int r 256)))

(defun rand-bytes (r n)
  (let ((v (make-array n :element-type '(unsigned-byte 8))))
    (dotimes (i n) (setf (aref v i) (rng-int r 256)))
    v))

;;; ---- host tree model ---------------------------------------------------------------
;;; A model entry is (REL KIND BYTES): REL "a/b", KIND :dir or :file, BYTES the
;;; contents of a file (NIL for a directory).

(defun name-alnum ()
  "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789")

(defun name-mid ()
  "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-_+(),   ")

(defun rng-char (r s) (char s (rng-int r (length s))))

(defun rand-name (r taken)
  "A name whose upcased form is not in TAKEN.  FAT is case-insensitive."
  (loop
    (let* ((x (rng-real r))
           (n (cond ((< x 0.3) (rng-range r 1 8))
                    ((< x 0.75) (rng-range r 9 24))
                    (t (rng-range r 25 80))))
           (name (make-string n)))
      (if (= n 1)
          (setf (char name 0) (rng-char r (name-alnum)))
          (progn
            (setf (char name 0) (rng-char r (name-alnum)))
            (loop for i from 1 below (- n 1)
                  do (setf (char name i) (rng-char r (name-mid))))
            (setf (char name (- n 1)) (rng-char r (name-alnum)))
            (when (and (>= n 6) (rng-chance r 0.5))
              (setf (char name (rng-range r 2 (- n 4))) #\.))))
      (unless (or (member (string-upcase name) taken :test #'string=)
                  (string= name ".") (string= name ".."))
        (return name)))))

(defun pick-size (r cs used budget)
  (if (> used budget)
      (rng-range r 0 512)
      (let ((x (rng-real r)))
        (cond ((< x 0.15) 0)
              ((< x 0.45) (rng-range r 1 8192))
              ((< x 0.70) (rng-range r 8193 200000))
              ((< x 0.85) (* cs (rng-range r 1 8)))
              (t (rng-range r 200000 1000000))))))

(defun join-rel (parent name)
  (if (string= parent "") name (concatenate 'string parent "/" name)))

(defun build-host-tree (r cs)
  "The model: a list of (REL KIND BYTES), directories before their contents."
  (let ((dirs (list ""))
        (taken (make-hash-table :test 'equal))
        (entries nil)
        (used 0)
        (budget (* 8 1024 1024)))
    (dotimes (i (rng-range r 0 6))
      (let* ((parent (rng-pick r dirs))
             (name (rand-name r (gethash parent taken)))
             (rel (join-rel parent name)))
        (push (string-upcase name) (gethash parent taken))
        (push rel dirs)
        (push (list rel :dir nil) entries)))
    (let* ((big (if (rng-chance r 0.25) (rng-pick r dirs) nil))
           (nfiles (rng-range r 3 25))
           (total (+ nfiles (if big (rng-range r 150 260) 0))))
      (dotimes (i total)
        (let* ((in-big (and big (>= i nfiles)))
               (parent (if in-big big (rng-pick r dirs)))
               (size (if in-big (rng-range r 0 64) (pick-size r cs used budget)))
               (name (rand-name r (gethash parent taken)))
               (rel (join-rel parent name)))
          (push (string-upcase name) (gethash parent taken))
          (push (list rel :file (rand-bytes r size)) entries)
          (incf used size))))
    (reverse entries)))

(defun host-path (src rel)
  (concatenate 'string src "/" rel))

(defun write-bytes-file (path bytes)
  (with-open-file (s path :direction :output :element-type '(unsigned-byte 8)
                          :if-exists :supersede)
    (write-sequence bytes s)))

(defun write-host-tree (src entries)
  (host-must *mkdir* "-p" src)
  (let ((dirs (loop for e in entries
                    when (eq (second e) :dir) collect (host-path src (first e)))))
    (when dirs (apply #'host-must *mkdir* "-p" dirs)))
  (dolist (e entries)
    (when (eq (second e) :file)
      (write-bytes-file (host-path src (first e)) (third e)))))

(defun build-image (src img spc entries)
  (host-must *mkfs* "-F" "32" "-s" (princ-to-string spc) "-n" "FUZZVOL" "-C" img
             (princ-to-string (if (= spc 1) 40960 70000)))
  (let ((tops (loop for e in entries
                    unless (find #\/ (first e)) collect (host-path src (first e)))))
    (when tops
      (apply #'host-must *mcopy* "-s" "-i" img (append tops (list "::"))))))

(defun delete-some (r src img entries)
  "Delete one or two random files from the image and from the host.  Returns the
   surviving model."
  (let ((files (remove-if-not (lambda (e) (eq (second e) :file)) entries)))
    (dotimes (k (rng-range r 0 2))
      (when files
        (let ((victim (rng-pick r files)))
          (setq files (remove victim files))
          (host-must *mdel* "-i" img (concatenate 'string "::" (first victim)))
          (host-must *rm* "-f" (host-path src (first victim)))
          (setq entries (remove victim entries)))))
    entries))

(defun read-image (path)
  (with-open-file (s path :element-type '(unsigned-byte 8))
    (let ((v (make-array (file-length s) :element-type '(unsigned-byte 8))))
      (read-sequence v s)
      v)))

;;; ---- the primary oracle: reader against model ---------------------------------------

(defun expected-files (entries)
  "(PATH FNV SIZE) for each model file."
  (loop for e in entries
        when (eq (second e) :file)
          collect (list (first e) (wk-fnv (third e)) (length (third e)))))

(defun actual-files (result)
  "(PATH FNV SIZE) for each file the walker reached."
  (loop for e in (getf result :entries)
        when (eq (first e) :file)
          collect (list (second e) (third e) (fourth e))))

(defun expected-dirs (entries)
  (cons "" (loop for e in entries when (eq (second e) :dir) collect (first e))))

(defun actual-dirs (result)
  (loop for e in (getf result :entries) when (eq (first e) :dir) collect (second e)))

(defun check-tree (entries result label &optional fold)
  "Problems, as strings, where RESULT (a walk-shaped plist) disagrees with the model
   ENTRIES.  LABEL names the observer in each message.  Empty when they agree.
   With FOLD, names compare case-insensitively: FAT is case-insensitive, and the
   case shown for an 8.3-compatible name depends on the NT flags the writer set."
  (let ((problems nil)
        (same (if fold #'string-equal #'string=))
        (want (expected-files entries))
        (got (actual-files result)))
    (unless (string= (getf result :status) "ok")
      (return-from check-tree
        (list (format nil "~a walk ~a: ~a" label (getf result :status) (getf result :msg)))))
    (dolist (w want)
      (let ((g (find (first w) got :key #'first :test same)))
        (cond ((null g)
               (push (format nil "~a: file missing: ~s" label (first w)) problems))
              ((not (equal (rest w) (rest g)))
               (push (format nil "~a: contents differ: ~s want (fnv size)=~s got ~s"
                             label (first w) (rest w) (rest g))
                     problems)))))
    (dolist (g got)
      (unless (find (first g) want :key #'first :test same)
        (push (format nil "~a: lists a file that is not there: ~s" label (first g)) problems)))
    (let ((wd (expected-dirs entries))
          (gd (actual-dirs result)))
      (dolist (d (set-difference wd gd :test same))
        (push (format nil "~a: directory missing: ~s" label d) problems))
      (dolist (d (set-difference gd wd :test same))
        (push (format nil "~a: lists a directory that is not there: ~s" label d) problems)))
    (nreverse problems)))

;;; ---- fuzz cases -------------------------------------------------------------------------
;;; A case is a plist: :id :cat :patches (OFFSET . VALUE)... :owner :cap :newsize.
;;; The base walk's entries give the layout: (:dir PATH CHAIN) and
;;; (:file PATH FNV SIZE CHAIN); the chain is always the last element.

(defun entry-path (e) (second e))
(defun entry-chain (e) (car (last e)))
(defun entry-size (e) (fourth e))

(defun g-bps (g) (nth 0 g))
(defun g-spc (g) (nth 1 g))
(defun g-fat-start (g) (nth 2 g))
(defun g-data-start (g) (nth 3 g))

(defun u16 (img off)
  (+ (aref img off) (* 256 (aref img (+ off 1)))))

(defun parent-path (p)
  (let ((slash (position #\/ p :from-end t)))
    (if slash (subseq p 0 slash) "")))

(defun find-dir-entry (entries path)
  (find-if (lambda (e) (and (eq (first e) :dir) (string= (entry-path e) path))) entries))

(defun find-size-field (img geom chain first)
  "Byte offset of the size field of the file entry whose first cluster is FIRST,
   searching the directory clusters in CHAIN.  NIL if not found."
  (let ((cs (* (g-bps geom) (g-spc geom))))
    (dolist (c chain)
      (dotimes (k (floor cs 32))
        (let* ((off (+ (g-data-start geom) (* (- c 2) cs) (* 32 k)))
               (b0 (aref img off))
               (attr (aref img (+ off 11))))
          (unless (or (member b0 '(0 229))
                      (= (logand attr #x3F) #x0F)
                      (/= 0 (logand attr #x10)))
            (when (= (+ (ash (u16 img (+ off 20)) 16) (u16 img (+ off 26))) first)
              (return-from find-size-field (+ off 28)))))))
    nil))

(defun bpb-case (r i)
  (let ((off (rng-pick r (append (loop for k below 90 collect k) (list 510 511)))))
    (list :id i :cat "bpb" :patches (list (cons off (rand-byte r)))
          :owner nil :cap nil :newsize nil)))

(defun data-case (r img geom files i)
  (let* ((owner (rng-pick r files))
         (size (entry-size owner)))
    (when (> size 0)
      (let* ((cs (* (g-bps geom) (g-spc geom)))
             (idx (rng-int r size))
             (c (nth (floor idx cs) (entry-chain owner)))
             (off (+ (g-data-start geom) (* (- c 2) cs) (mod idx cs)))
             (new (mod (+ (aref img off) (rng-range r 1 255)) 256)))
        (list :id i :cat "data" :patches (list (cons off new))
              :owner (entry-path owner) :cap nil :newsize nil)))))

(defun size-case (r img geom entries files i)
  (let* ((owner (rng-pick r files))
         (chain (entry-chain owner))
         (cs (* (g-bps geom) (g-spc geom)))
         (cap (* (length chain) cs))
         (pdir (find-dir-entry entries (parent-path (entry-path owner))))
         (field (and pdir (find-size-field img geom (entry-chain pdir) (first chain))))
         (new (rng-pick r (list (+ cap 1) #xFFFFFFFF #x7FFFFFFF (rng-int r (+ cap 1))))))
    (when (and field (/= new (entry-size owner)))
      (list :id i :cat "size"
            :patches (loop for k below 4
                           collect (cons (+ field k) (logand (ash new (* -8 k)) 255)))
            :owner (entry-path owner) :cap cap :newsize new))))

(defun fat-case (r geom clusters i)
  (let ((c (rng-pick r clusters)))
    (list :id i :cat "fat"
          :patches (list (cons (+ (g-fat-start geom) (* c 4) (rng-int r 4)) (rand-byte r)))
          :owner nil :cap nil :newsize nil)))

(defun dirent-case (r geom dirs i)
  (let* ((cs (* (g-bps geom) (g-spc geom)))
         (c (rng-pick r (entry-chain (rng-pick r dirs))))
         (base (+ (g-data-start geom) (* (- c 2) cs) (* 32 (rng-int r (floor cs 32))))))
    (list :id i :cat "dirent"
          :patches (loop repeat (rng-range r 1 3)
                         collect (cons (+ base (rng-int r 32)) (rand-byte r)))
          :owner nil :cap nil :newsize nil)))

(defun gen-case (r img geom entries files dirs clusters i cat)
  (or (cond ((and (string= cat "size") files) (size-case r img geom entries files i))
            ((and (string= cat "data") files) (data-case r img geom files i))
            ((and (string= cat "fat") clusters) (fat-case r geom clusters i))
            ((and (string= cat "dirent") dirs) (dirent-case r geom dirs i))
            (t nil))
      (bpb-case r i)))

(defun case-pool ()
  (append (make-list 40 :initial-element "data")
          (make-list 15 :initial-element "size")
          (make-list 15 :initial-element "fat")
          (make-list 20 :initial-element "dirent")
          (make-list 10 :initial-element "bpb")))

(defun gen-cases (r img geom entries n)
  (let* ((files (remove-if-not (lambda (e) (and (eq (first e) :file) (entry-chain e))) entries))
         (dirs (remove-if-not (lambda (e) (and (eq (first e) :dir) (entry-chain e))) entries))
         (clusters (loop for e in entries append (entry-chain e)))
         (pool (case-pool))
         (cases nil))
    (dotimes (i n)
      (push (gen-case r img geom entries files dirs clusters (+ i 1) (rng-pick r pool))
            cases))
    (reverse cases)))

;;; ---- judging ----------------------------------------------------------------------------

(defun files-changed (base result)
  "Paths whose (fnv size) differs between BASE and RESULT, or that appear in only one."
  (let ((b (actual-files base))
        (g (actual-files result))
        (out nil))
    (dolist (x b)
      (let ((y (find (first x) g :key #'first :test #'string=)))
        (unless (and y (equal (rest x) (rest y)))
          (push (first x) out))))
    (dolist (y g)
      (unless (find (first y) b :key #'first :test #'string=)
        (push (first y) out)))
    out))

(defun only-owner-changed-p (c base result)
  (let ((changed (files-changed base result)))
    (and (= (length changed) 1)
         (string= (first changed) (getf c :owner)))))

(defun same-dirs-p (base result)
  (let ((a (actual-dirs base)) (b (actual-dirs result)))
    (and (null (set-difference a b :test #'string=))
         (null (set-difference b a :test #'string=)))))

(defun judge (c result base)
  "(values VERDICT DETAIL), VERDICT \"PASS\" or \"FAIL\"."
  (let ((status (getf result :status))
        (msg (getf result :msg))
        (cat (getf c :cat)))
    (cond
      ((string= status "internal")
       (values "FAIL" (format nil "internal error: ~a" (subseq msg 0 (min 300 (length msg))))))
      ((string= cat "data")
       (cond ((not (string= status "ok"))
              (values "FAIL" (format nil "data-byte flip gave ~a: ~a" status msg)))
             ((not (only-owner-changed-p c base result))
              (values "FAIL" (format nil "changed ~s, expected only ~s"
                                     (files-changed base result) (getf c :owner))))
             ((not (same-dirs-p base result))
              (values "FAIL" "data-byte flip changed the directory structure"))
             (t (values "PASS" ""))))
      ((string= cat "size")
       (cond ((> (getf c :newsize) (getf c :cap))
              (if (string= status "fat32-error")
                  (values "PASS" "")
                  (values "FAIL" (format nil "size ~a exceeds chain capacity ~a, got ~a"
                                         (getf c :newsize) (getf c :cap) status))))
             ((not (string= status "ok"))
              (values "FAIL" (format nil "size ~a fits chain ~a, got ~a: ~a"
                                     (getf c :newsize) (getf c :cap) status msg)))
             ((not (only-owner-changed-p c base result))
              (values "FAIL" (format nil "changed ~s, expected only ~s"
                                      (files-changed base result) (getf c :owner))))
             (t (values "PASS" ""))))
      ((member status '("ok" "fat32-error" "limit") :test #'string=)
       (values "PASS" ""))
      (t (values "FAIL" (format nil "unexpected status ~a" status))))))

;;; ---- tallies and reporting -------------------------------------------------------------

(defun tally (tbl key sub)
  (let* ((cell (assoc sub (gethash key tbl) :test #'string=)))
    (if cell
        (incf (cdr cell))
        (push (cons sub 1) (gethash key tbl)))))

(defun tally-count (tbl key sub)
  (or (cdr (assoc sub (gethash key tbl) :test #'string=)) 0))

(defun tally-statuses (tbl key)
  "Cases in KEY: the sum of the four status buckets, not the verdicts too."
  (reduce #'+ (mapcar (lambda (s) (tally-count tbl key s))
                      '("ok" "fat32-error" "limit" "internal"))
          :initial-value 0))

(defun print-top (tbl key n)
  (let ((rows (sort (copy-list (gethash key tbl)) #'> :key #'cdr)))
    (dolist (row (subseq rows 0 (min n (length rows))))
      (format t "    ~4d x ~a~%" (cdr row) (car row)))))

(defun write-lines (path lines)
  (with-open-file (s path :direction :output :if-exists :supersede)
    (dolist (l lines) (format s "~a~%" l))))

;;; ---- driver ----------------------------------------------------------------------------

(defun run-seed (seed ncases workdir)
  "Run one seed.  Returns T when every check passes."
  (let* ((r (rng-make seed))
         (spc (rng-pick r '(1 2)))
         (cs (* 512 spc))
         (dir (concatenate 'string workdir "/seed-" (princ-to-string seed)))
         (src (concatenate 'string dir "/src"))
         (img (concatenate 'string dir "/base.img"))
         (failures nil))
    (host-must *mkdir* "-p" dir)
    (let* ((entries (build-host-tree r cs)))
      (write-host-tree src entries)
      (build-image src img spc entries)
      (setq entries (delete-some r src img entries))
      (let ((fsck-code (host-run *fsck* "-n" img))
            (nfiles (count :file entries :key #'second))
            (ndirs (count :dir entries :key #'second)))
        (format t "seed ~a: spc=~a files=~a dirs=~a~%" seed spc nfiles ndirs)
        (unless (= fsck-code 0)
          (push (format nil "fsck.fat -n rejected the image (exit ~a)" fsck-code) failures))
        (let* ((bytes (read-image img))
               (base (wk-run bytes nil))
               (problems (check-tree entries base "base")))
          (when problems
            (dolist (p problems) (push (format nil "base: ~a" p) failures)))
          (cond
            (problems
             (format t "  base: FAIL (~a problems)~%" (length problems)))
            (t
             (format t "  base: reader agrees with the mkfs+mtools oracle (fsck ~a)~%"
                     (if (= fsck-code 0) "clean" "REJECTED"))
             (let* ((geom (getf base :geom))
                    (cases (gen-cases r bytes geom (getf base :entries) ncases))
                    (counts (make-hash-table :test 'equal))
                    (msgs (make-hash-table :test 'equal)))
               (dolist (c cases)
                 (let ((res (wk-run bytes (getf c :patches))))
                   (multiple-value-bind (verdict detail) (judge c res base)
                     (tally counts (getf c :cat) (getf res :status))
                     (tally counts (getf c :cat) verdict)
                     (when (string= (getf res :status) "fat32-error")
                       (tally msgs "fat32" (subseq (getf res :msg)
                                                   0 (min 90 (length (getf res :msg))))))
                     (when (string= verdict "FAIL")
                       (push (format nil "case ~a [~a] ~a  patches=~s"
                                     (getf c :id) (getf c :cat) detail (getf c :patches))
                             failures)))))
               (format t "  fuzz: ~a cases~%" (length cases))
               (dolist (cat '("data" "size" "fat" "dirent" "bpb"))
                 (when (gethash cat counts)
                   (format t "    ~7a n=~4d ok=~4d fat32-error=~4d limit=~3d internal=~3d FAIL=~d~%"
                           cat (tally-statuses counts cat)
                           (tally-count counts cat "ok")
                           (tally-count counts cat "fat32-error")
                           (tally-count counts cat "limit")
                           (tally-count counts cat "internal")
                           (tally-count counts cat "FAIL"))))
               (when (gethash "fat32" msgs)
                 (format t "  most common fat32-error messages:~%")
                 (print-top msgs "fat32" 5)))))
          (cond
            (failures
             (let ((path (concatenate 'string dir "/failures.txt")))
               (write-lines path (reverse failures))
               (format t "  FAILURES: ~a (first 10 shown; all in ~a)~%" (length failures) path)
               (dolist (f (subseq (reverse failures) 0 (min 10 (length failures))))
                 (format t "    ~a~%" f))
               nil))
            (t (format t "  PASS~%") t)))))))

(defun oracle-main ()
  (let ((ok t))
    (dotimes (k *oracle-seeds*)
      (unless (run-seed (+ *oracle-first-seed* k) *oracle-cases* *oracle-workdir*)
        (setq ok nil)))
    (format t "FAT32-ORACLE ~a~%" (if ok "PASS" "FAIL"))))

;; Loaded as a library (test/fat32-ops.lisp) when *oracle-library-only* is bound.
(unless (boundp '*oracle-library-only*)
  (oracle-main))
