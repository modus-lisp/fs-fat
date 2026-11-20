;;;; test/fat12-test.lisp -- FAT12 and FAT16 (FAT-OPEN, FAT-FORMAT-FLOPPY) against
;;;; the host's tools: mformat / mkfs.vfat make the volumes, mcopy fills and
;;;; empties them, fsck.fat -n judges what this library wrote.
;;;;
;;;;   test/run-fat12.sh
;;;;
;;;; For each volume -- a 1.44 MB and a 720 KB floppy by mformat, a 16 MB FAT16 by
;;;; mkfs.vfat, and a 1.44 MB and a 720 KB floppy by FAT-FORMAT-FLOPPY:
;;;;  1. mcopy puts a tree in: the root and a subdirectory, long and short names,
;;;;     an empty file, a one-byte file, and a 300 KB file (on a floppy its FAT
;;;;     entries run past the first FAT sector, and a 12-bit entry straddles one);
;;;;     FAT-OPEN reads every file back byte for byte, and lists both directories.
;;;;  2. This library writes: a new 200 KB file, a new directory with a file in
;;;;     it, a file deleted, a file renamed into the directory; fsck.fat -n must
;;;;     accept the volume, and mcopy must read back what was written.
;;;;  3. On the floppies, the fixed root fills: files are written into it until it
;;;;     signals, and that must happen at the root's entry count, with the volume
;;;;     still clean.
;;;; The last line is "FAT12 PASS" or "FAT12 FAIL".
;;;;
;;;; HOST-RUN is the one host seam (sb-ext:run-program); *ROOT* and *WORK* come
;;;; from test/run-fat12.sh.

(load (concatenate 'string *root* "lib/fat32.lisp"))
(use-package :fs-fat)

(defvar *checks* 0)
(defvar *fails* 0)
(defun check (ok label)
  (incf *checks*)
  (unless ok (incf *fails*) (format t "FAIL: ~a~%" label))
  ok)

(defun host-run (program &rest args)
  "Run PROGRAM; (values exit-code output)."
  (let* ((out (make-string-output-stream))
         (p (sb-ext:run-program program args :search t :output out :error out)))
    (values (sb-ext:process-exit-code p) (get-output-stream-string out))))

(defun work (name) (concatenate 'string *work* name))

(defun read-file (path)
  (with-open-file (in path :element-type '(unsigned-byte 8))
    (let ((v (make-array (file-length in) :element-type '(unsigned-byte 8))))
      (read-sequence v in) v)))

(defun write-file (path v)
  (with-open-file (o path :element-type '(unsigned-byte 8) :direction :output :if-exists :supersede)
    (write-sequence v o)))

(defun bytes (n seed)
  (let ((v (make-array n :element-type '(unsigned-byte 8))) (x seed))
    (dotimes (i n v)
      (setq x (mod (+ (* x 1103515245) 12345) 2147483648))
      (setf (aref v i) (ldb (byte 8 16) x)))))

(defparameter *tree*
  ;; (path . contents): the files mcopy puts in
  (list (cons "README.TXT" (map '(vector (unsigned-byte 8)) #'char-code "a floppy for a Mac"))
        (cons "a long file name.lisp" (bytes 5000 1))
        (cons "EMPTY" (make-array 0 :element-type '(unsigned-byte 8)))
        (cons "one" (bytes 1 2))
        (cons "big.bin" (bytes 300000 3))
        (cons "SUB/inner file.txt" (bytes 1500 4))
        (cons "SUB/KEY.PEM" (bytes 777 5))))

(defun mcopy-in (img)
  (host-run "mmd" "-i" img "::SUB")
  (dolist (f *tree*)
    (let ((host (work "in.tmp")))
      (write-file host (cdr f))
      (host-run "mcopy" "-i" img host (concatenate 'string "::" (car f))))))

(defun mcopy-out (img path)
  "PATH's bytes as mcopy reads them from IMG, or NIL."
  (let ((host (work "out.tmp")))
    (when (probe-file host) (delete-file host))
    (host-run "mcopy" "-n" "-i" img (concatenate 'string "::" path) host)
    (and (probe-file host) (read-file host))))

(defun fsck-clean-p (img)
  (multiple-value-bind (code out) (host-run "/sbin/fsck.fat" "-n" img)
    (or (= code 0) (progn (format t "~a~%" out) nil))))

(defun exercise (label img expect-type root-entries)
  (format t "~&-- ~a~%" label)
  (mcopy-in img)
  (check (fsck-clean-p img) (format nil "~a: mtools' volume is clean" label))
  (let* ((bytes (read-file img))
         (v (fat-open bytes)))
    (check (= (fat-type v) expect-type) (format nil "~a: FAT~a, read as FAT~a" label expect-type (fat-type v)))
    ;; 1. every file reads back, both directories list
    (dolist (f *tree*)
      (check (equalp (fat32-read-file v (car f)) (cdr f)) (format nil "~a: read ~a" label (car f))))
    (check (equal (sort (mapcar #'first (fat32-directory v "/SUB")) #'string<)
                  (sort (list "inner file.txt" "KEY.PEM") #'string<))
           (format nil "~a: SUB lists" label))
    (check (= (length (fat32-directory v "/")) 6) (format nil "~a: the root lists 6" label))
    ;; 2. writes
    (let ((new (bytes 200000 6)) (inner (bytes 3000 7)))
      (fat32-write-file v "/written by modus.bin" new)
      (fat32-mkdir v "/KEYS")
      (fat32-write-file v "/KEYS/root key.txt" inner)
      (fat32-delete v "/one")
      (fat32-rename v "/README.TXT" "/KEYS/README.TXT")
      (write-file img bytes)
      (check (fsck-clean-p img) (format nil "~a: clean after our writes" label))
      (check (equalp (mcopy-out img "written by modus.bin") new) (format nil "~a: mcopy reads our 200 KB file" label))
      (check (equalp (mcopy-out img "KEYS/root key.txt") inner) (format nil "~a: mcopy reads our file in our directory" label))
      (check (null (mcopy-out img "one")) (format nil "~a: the deleted file is gone" label))
      (check (equalp (mcopy-out img "KEYS/README.TXT") (cdr (first *tree*))) (format nil "~a: the renamed file" label))
      (check (equalp (mcopy-out img "big.bin") (cdr (assoc "big.bin" *tree* :test #'string=)))
             (format nil "~a: big.bin untouched" label)))
    ;; 3. the fixed root fills at its entry count
    (when root-entries
      (let* ((v (fat-open bytes)) (made 0))
        (handler-case
            (loop (fat32-write-file v (format nil "/F~d" made) (bytes 10 made)) (incf made))
          (error () nil))
        ;; every slot in use (long names and a label take slots too) when it signals
        (let* ((raw (nth-value 1 (fs-fat::%fat-scan-dir v -1)))
               (used (loop for i from 0 below (length raw) by 32
                           count (not (member (aref raw i) '(0 #xE5))))))
          (check (and (> made 100) (= used root-entries))
                 (format nil "~a: the root filled at ~a slots (~a files written), its count is ~a"
                         label used made root-entries)))
        (write-file img bytes)
        (check (fsck-clean-p img) (format nil "~a: clean with a full root" label))
        (check (equalp (mcopy-out img (format nil "F~d" (- made 1))) (bytes 10 (- made 1)))
               (format nil "~a: mcopy reads the root's last file" label))))))

(defun blank (img size)
  (write-file img (make-array size :element-type '(unsigned-byte 8) :initial-element 0)))

;; mtools' floppies
(blank (work "m144.img") 1474560)
(host-run "mformat" "-i" (work "m144.img") "-f" "1440" "::")
(exercise "mformat 1.44 MB" (work "m144.img") 12 224)
(blank (work "m720.img") 737280)
(host-run "mformat" "-i" (work "m720.img") "-f" "720" "::")
(exercise "mformat 720 KB" (work "m720.img") 12 112)
;; mkfs.vfat's FAT16
(blank (work "f16.img") (* 16 1024 1024))
(host-run "/sbin/mkfs.vfat" "-F" "16" (work "f16.img"))
(exercise "mkfs.vfat -F 16, 16 MB" (work "f16.img") 16 nil)
;; ours
(dolist (size '(1474560 737280))
  (let* ((img (work (format nil "ours~d.img" size)))
         (dev (make-fat32-device (make-array size :initial-element 0))))
    (fat-format-floppy dev :label "MODUS" :volume-id #x1234ABCD)
    (write-file img (coerce (device-read dev 0 size) '(vector (unsigned-byte 8))))
    (check (fsck-clean-p img) (format nil "fat-format-floppy ~d: clean when empty" size))
    (multiple-value-bind (code out) (host-run "minfo" "-i" img "::")
      (declare (ignore code))
      (check (search "MODUS" out) (format nil "fat-format-floppy ~d: minfo sees the label" size)))
    (exercise (format nil "fat-format-floppy ~d" size) img 12 (if (= size 1474560) 224 112))))

(format t "~&~d checks, ~d failed~%FAT12 ~a~%" *checks* *fails* (if (zerop *fails*) "PASS" "FAIL"))
