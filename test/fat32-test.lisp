;;;; test/fat32-test.lisp -- lib/fat32.lisp against a volume made by mkfs.vfat and
;;;; filled by mtools.  Portable CL: nothing here is implementation-specific.
;;;;
;;;; The host-side runner (test/run-fat32.sh) defines these before loading this
;;;; file, because portable CL has no command-line access:
;;;;   *fat-lib*       path of lib/fat32.lisp
;;;;   *fat-image*     path of the FAT32 image
;;;;   *fat-manifest*  path of a file of lines "TYPE RELPATH", TYPE d or f
;;;;   *fat-src*       the source tree the image was filled from
;;;;
;;;; The source tree is the ground truth: every file's bytes and every
;;;; directory's child names are compared against it.  The final line is
;;;; "FAT32-TEST PASS" or "FAT32-TEST FAIL".

(load *fat-lib*)
(use-package :fs-fat)

(defvar *fat-checks* 0)
(defvar *fat-fails* 0)

(defun fat-check (ok label)
  (setq *fat-checks* (+ *fat-checks* 1))
  (unless ok
    (setq *fat-fails* (+ *fat-fails* 1))
    (format t "FAIL: ~a~%" label))
  ok)

(defun fat-read-bytes (path)
  (with-open-file (s path :element-type '(unsigned-byte 8))
    (let* ((n (file-length s))
           (v (make-array n :element-type '(unsigned-byte 8))))
      (read-sequence v s)
      v)))

(defun fat-same-bytes (a b)
  (and (= (length a) (length b))
       (let ((i 0) (n (length a)) (same t))
         (loop
           (when (>= i n) (return))
           (unless (= (aref a i) (aref b i))
             (setq same nil)
             (return))
           (setq i (+ i 1)))
         same)))

(defun fat-parent (rel)
  (let ((slash (position #\/ rel :from-end t)))
    (if slash (subseq rel 0 slash) "")))

(defun fat-base (rel)
  (let ((slash (position #\/ rel :from-end t)))
    (if slash (subseq rel (+ slash 1)) rel)))

(defun fat-read-manifest (path)
  "List of (TYPE . RELPATH), TYPE a character, in file order."
  (with-open-file (s path)
    (let ((acc nil))
      (loop
        (let ((line (read-line s nil nil)))
          (when (null line) (return))
          (setq acc (cons (cons (char line 0) (subseq line 2)) acc))))
      (nreverse acc))))

(defun fat-names (entries)
  (mapcar (lambda (e) (nth 0 e)) entries))

(defun fat-run ()
  (let* ((image (fat-read-bytes *fat-image*))
         (vol (fat32-open image))
         (manifest (fat-read-manifest *fat-manifest*))
         (dirs (cons "" nil)))
    (format t "volume: ~a bytes, ~a entries in the manifest~%"
            (length image) (length manifest))

    ;; Every file reads back byte-for-byte, with the right size, and every
    ;; name resolves case-insensitively to the same entry.
    (dolist (m manifest)
      (let ((rel (cdr m)))
        (cond
          ((char= (car m) #\d)
           (setq dirs (cons rel dirs))
           (fat-check (let ((e (fat32-lookup vol rel))) (and e (nth 1 e)))
                      (format nil "directory present: ~a" rel)))
          (t
           (let* ((data (fat-read-bytes (concatenate 'string *fat-src* "/" rel)))
                  (got (fat32-read-file vol rel))
                  (e (fat32-lookup vol rel))
                  (u (fat32-lookup vol (string-upcase rel))))
             (fat-check (and got (fat-same-bytes got data))
                        (format nil "contents: ~a" rel))
             (fat-check (and e (= (nth 2 e) (length data)))
                        (format nil "size: ~a" rel))
             (fat-check (and e u (= (nth 3 e) (nth 3 u)))
                        (format nil "case-insensitive lookup: ~a" rel)))))))

    ;; Every directory lists exactly its source children, by exact name.
    (dolist (d dirs)
      (let* ((children (mapcar #'cdr
                               (remove-if-not (lambda (m)
                                                (string= (fat-parent (cdr m)) d))
                                              manifest)))
             (listing (fat32-directory vol d))
             (names (fat-names listing)))
        (fat-check listing (format nil "directory readable: ~s" d))
        (fat-check (= (length names) (length children))
                   (format nil "child count in ~s: fat=~a source=~a"
                           d (length names) (length children)))
        (dolist (c children)
          (fat-check (member (fat-base c) names :test #'string=)
                     (format nil "child ~s listed in ~s" c d)))))

    ;; Negative cases.
    (fat-check (null (fat32-lookup vol "no-such-file")) "missing name is NIL")
    (fat-check (null (fat32-read-file vol "sub")) "read-file of a directory is NIL")
    (fat-check (null (fat32-lookup vol "small.txt/x")) "file used as a directory is NIL")
    (fat-check (null (fat32-lookup vol "gone.txt")) "deleted entry is not listed")
    (fat-check (= 0 (length (fat32-read-file vol "empty.bin")))
               "empty file reads as zero bytes")

    (format t "FAT32-TEST ~a checks=~a fails=~a~%"
            (if (= *fat-fails* 0) "PASS" "FAIL") *fat-checks* *fat-fails*)))

(fat-run)
