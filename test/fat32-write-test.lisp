;;;; test/fat32-write-test.lisp -- lib/fat32.lisp writes, applied to a volume and to a
;;;; host mirror in step.  Portable CL.  The runner (test/run-fat32-write.sh) then
;;;; checks the image with fsck.fat, exports it with mtools, and diffs the export
;;;; against the mirror.
;;;;
;;;; The runner defines before loading this file:
;;;;   *write-lib*     path of lib/fat32.lisp
;;;;   *write-image*   a freshly made, empty FAT32 image (rewritten in place)
;;;;   *write-mirror*  an empty directory to mirror every change into
;;;;
;;;; The workload exercises what the random operations rarely reach: a directory
;;;; that spans several clusters, multi-cluster files, replacing a large file with a
;;;; small one, freeing many clusters, and removing a nested tree.

(load *write-lib*)
(use-package :fs-fat)

(defun wt-read (path)
  (with-open-file (s path :element-type '(unsigned-byte 8))
    (let ((v (make-array (file-length s) :element-type '(unsigned-byte 8))))
      (read-sequence v s)
      v)))

(defun wt-write (path bytes)
  (with-open-file (s path :direction :output :element-type '(unsigned-byte 8)
                          :if-exists :supersede)
    (write-sequence bytes s)))

(defun wt-pattern (n seed)
  (let ((v (make-array n :element-type '(unsigned-byte 8))))
    (dotimes (i n) (setf (aref v i) (mod (+ (* i 31) seed) 251)))
    v))

(defvar *wt-v* nil)
(defvar *wt-files* nil "The expected final files: an alist of (REL . BYTES).")
(defvar *wt-dirs* nil "The expected final directories, as REL strings.")

;;; The mirror is built once, at the end, from what should remain.  That keeps the
;;; test to portable CL: no host-side directory removal is needed.

(defun wt-put (rel bytes)
  (fat32-write-file *wt-v* rel bytes)
  (setq *wt-files* (cons (cons rel bytes)
                         (remove rel *wt-files* :key #'car :test #'string=))))

(defun wt-mkdir (rel)
  (fat32-mkdir *wt-v* rel)
  (setq *wt-dirs* (cons rel *wt-dirs*)))

(defun wt-rm (rel)
  (fat32-delete *wt-v* rel)
  (setq *wt-files* (remove rel *wt-files* :key #'car :test #'string=)))

(defun wt-rmdir (rel)
  (fat32-delete *wt-v* rel)
  (setq *wt-dirs* (remove rel *wt-dirs* :test #'string=)))

(defun wt-write-mirror ()
  (dolist (d (reverse *wt-dirs*))
    (ensure-directories-exist (concatenate 'string *write-mirror* "/" d "/")))
  (dolist (f (reverse *wt-files*))
    (let ((path (concatenate 'string *write-mirror* "/" (car f))))
      (ensure-directories-exist path)
      (wt-write path (cdr f)))))

(defun wt-main ()
  (let* ((bytes (wt-read *write-image*))
         (*wt-v* (fat32-open bytes)))
    (wt-mkdir "many")
    (dotimes (i 300)
      (let ((name (format nil "many/file-~3,'0d-with-a-longer-name.dat" (+ i 1))))
        (wt-put name (wt-pattern (* 400 (mod (+ i 1) 7)) (+ i 1)))))
    (wt-put "big.bin" (wt-pattern 3145728 7))
    (wt-mkdir "sub")
    (wt-mkdir "sub/deeper")
    (wt-put "sub/deeper/x.txt" (wt-pattern 10 3))
    (wt-put "Mixed Case.Txt" (wt-pattern 33 5))
    (wt-put "lower.txt" (wt-pattern 5 9))
    (wt-put "UPPER.TXT" (wt-pattern 7 11))
    (wt-put "empty.bin" (make-array 0 :element-type '(unsigned-byte 8)))
    ;; name shapes: several dots, a leading dot, exact 8.3, and a name long enough
    ;; to need sixteen long-name entries, which spans directory clusters
    (wt-put "a.b.c" (wt-pattern 3 17))
    (wt-put ".hidden" (wt-pattern 4 19))
    (wt-put "ABCDEFGH.TXT" (wt-pattern 8 21))
    (wt-put (make-string 200 :initial-element #\n) (wt-pattern 12 23))
    ;; replace a large file with a small one, and a small one with a large one
    (wt-put "big.bin" (wt-pattern 100000 13))
    (wt-put "lower.txt" (wt-pattern 5000 15))
    ;; free many clusters and directory slots
    (dotimes (i 150)
      (when (evenp (+ i 1))
        (wt-rm (format nil "many/file-~3,'0d-with-a-longer-name.dat" (+ i 1)))))
    (wt-rm "sub/deeper/x.txt")
    (wt-rmdir "sub/deeper")
    (wt-rmdir "sub")
    (wt-write *write-image* bytes)
    (wt-write-mirror)
    (format t "WRITE-DONE~%")))

(wt-main)
