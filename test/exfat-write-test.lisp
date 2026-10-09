;;;; test/exfat-write-test.lisp -- lib/exfat.lisp writes, checked against a host mirror.
;;;; Portable CL.  The runner (test/run-exfat-write.sh) makes an empty exFAT image with
;;;; mkfs.exfat, runs this, and then runs fsck.exfat -n on the result.
;;;;
;;;; The runner defines before loading this file:
;;;;   *write-lib*     path of lib/fat32.lisp
;;;;   *exfat-lib*     path of lib/exfat.lisp
;;;;   *write-image*   the empty image (rewritten in place)
;;;;   *write-mirror*  an empty directory: the expected final tree is written there
;;;;
;;;; Every change is made to the image and recorded in memory.  At the end the image is
;;;; re-read from the file, and each expected file and directory is checked against it.

(load *write-lib*)
(use-package :fs-fat)
(load *exfat-lib*)

(defun xt-read (path)
  (with-open-file (s path :element-type '(unsigned-byte 8))
    (let ((v (make-array (file-length s) :element-type '(unsigned-byte 8))))
      (read-sequence v s)
      v)))

(defun xt-write (path bytes)
  (with-open-file (s path :direction :output :element-type '(unsigned-byte 8)
                          :if-exists :supersede)
    (write-sequence bytes s)))

(defun xt-pattern (n seed)
  (let ((v (make-array n :element-type '(unsigned-byte 8))))
    (dotimes (i n) (setf (aref v i) (mod (+ (* i 31) seed) 251)))
    v))

(defvar *xt-v* nil)
(defvar *xt-files* nil "Expected final files: an alist of (PATH . BYTES).")
(defvar *xt-dirs* nil "Expected final directories, as paths.")

(defun xt-put (path bytes)
  (exfat-write-file *xt-v* path bytes)
  (setq *xt-files* (cons (cons path bytes)
                         (remove path *xt-files* :key #'car :test #'string=))))

(defun xt-mkdir (path)
  (exfat-mkdir *xt-v* path)
  (setq *xt-dirs* (cons path *xt-dirs*)))

(defun xt-rm (path)
  (exfat-delete *xt-v* path)
  (setq *xt-files* (remove path *xt-files* :key #'car :test #'string=)))

(defun xt-rmdir (path)
  (exfat-delete *xt-v* path)
  (setq *xt-dirs* (remove path *xt-dirs* :test #'string=)))

(defun xt-under (path prefix)
  "PATH with PREFIX replaced by NEW-PREFIX is done by the caller; this tests PREFIX."
  (or (string= path prefix)
      (and (> (length path) (length prefix))
           (string= prefix path :end2 (length prefix))
           (char= (char path (length prefix)) #\/))))

(defun xt-move (old new)
  "Rename OLD to NEW on the volume and in the model.  A directory takes its contents
   with it."
  (exfat-rename *xt-v* old new)
  (flet ((renamed (p) (concatenate 'string new (subseq p (length old)))))
    (setq *xt-files* (mapcar (lambda (f)
                               (if (xt-under (car f) old)
                                   (cons (renamed (car f)) (cdr f))
                                   f))
                             *xt-files*))
    (setq *xt-dirs* (mapcar (lambda (d) (if (xt-under d old) (renamed d) d)) *xt-dirs*))))

(defun xt-parent (path)
  (let ((slash (position #\/ path :from-end t)))
    (if slash (subseq path 0 slash) "")))

(defun xt-check (ok label)
  (unless ok
    (format t "FAIL: ~a~%" label))
  ok)

(defun xt-verify (bytes)
  "Check the image BYTES against the expected tree.  Returns the number of failures."
  (let ((v (exfat-open bytes))
        (fails 0))
    (dolist (f *xt-files*)
      (let ((got (exfat-read-file v (car f))))
        (unless (and got (= (length got) (length (cdr f)))
                     (every #'= got (cdr f)))
          (incf fails)
          (format t "FAIL: contents of ~s~%" (car f)))))
    (dolist (d (cons "" *xt-dirs*))
      (let* ((listed (mapcar #'first (exfat-directory v d)))
             (want (append
                    (mapcar (lambda (f) (base-of (car f)))
                            (remove-if-not (lambda (f) (string= (xt-parent (car f)) d)) *xt-files*))
                    (mapcar #'base-of
                            (remove-if-not (lambda (x) (string= (xt-parent x) d)) *xt-dirs*)))))
        (unless (and (= (length listed) (length want))
                     (every (lambda (w) (member w listed :test #'string=)) want))
          (incf fails)
          (format t "FAIL: listing of ~s: got ~s, want ~s~%" d listed want))))
    fails))

(defun base-of (path)
  (let ((slash (position #\/ path :from-end t)))
    (if slash (subseq path (+ slash 1)) path)))

(defun xt-main ()
  (let* ((bytes (xt-read *write-image*))
         (*xt-v* (exfat-open bytes)))
    (xt-mkdir "many")
    (dotimes (i 300)
      (xt-put (format nil "many/file-~3,'0d-with-a-longer-name.dat" (+ i 1))
              (xt-pattern (* 400 (mod (+ i 1) 7)) (+ i 1))))
    (xt-put "big.bin" (xt-pattern 3145728 7))
    (xt-mkdir "sub")
    (xt-mkdir "sub/deeper")
    (xt-put "sub/deeper/x.txt" (xt-pattern 10 3))
    (xt-put "Mixed Case.Txt" (xt-pattern 33 5))
    (xt-put "lower.txt" (xt-pattern 5 9))
    (xt-put "UPPER.TXT" (xt-pattern 7 11))
    (xt-put "empty.bin" (make-array 0 :element-type '(unsigned-byte 8)))
    (xt-put "a.b.c" (xt-pattern 3 17))
    (xt-put ".hidden" (xt-pattern 4 19))
    (xt-put (make-string 200 :initial-element #\n) (xt-pattern 12 23))
    ;; replace a large file with a small one, and a small one with a large one
    (xt-put "big.bin" (xt-pattern 100000 13))
    (xt-put "lower.txt" (xt-pattern 5000 15))
    ;; free many entries and clusters
    (dotimes (i 150)
      (when (evenp (+ i 1))
        (xt-rm (format nil "many/file-~3,'0d-with-a-longer-name.dat" (+ i 1)))))
    ;; move things between directories
    (xt-move "sub/deeper/x.txt" "moved.txt")
    (xt-move "many/file-001-with-a-longer-name.dat" "sub/renamed-into-sub.dat")
    (xt-rmdir "sub/deeper")
    (xt-move "sub" "many/sub")
    (xt-write *write-image* bytes)
    (let ((fails (xt-verify bytes)))
      (format t "EXFAT-WRITE-VERIFY files=~a dirs=~a failures=~a~%"
              (length *xt-files*) (length *xt-dirs*) fails)
      (format t "~a~%" (if (= fails 0) "EXFAT-WRITE-TEST PASS" "EXFAT-WRITE-TEST FAIL")))))

(xt-main)
