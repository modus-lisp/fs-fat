;;;; test/volume-samples-test.lisp -- the readers against two volumes written by other
;;;; tools: the exFAT and FAT partitions of the Debian forensics-samples images, with
;;;; their original files.  Portable CL.
;;;;
;;;; The runner (test/run-volume-samples.sh) fetches and extracts the images, and
;;;; defines before loading this file:
;;;;   *vs-fat-lib*  *vs-exfat-lib*   the libraries (fat32.lisp first)
;;;;   *vs-exfat-part*  *vs-fat-part* the partitions, as raw volumes
;;;;   *vs-original*  the directory of original files
;;;;   *vs-export*    an empty directory: the FAT reader's tree is written here, for
;;;;                  the runner to compare with mtools' export of the same volume
;;;;
;;;; Checks: both volumes are read; every file reads the same from both; every file
;;;; matches its original, except the two PNGs that differ from their originals in both
;;;; images (pinned here as known, so any other difference fails); every exFAT file is
;;;; contiguous (NoFatChain).

(load *vs-fat-lib*)
(use-package :fs-fat)
(load *vs-exfat-lib*)

(defvar *vs-fails* 0)
(defvar *vs-known-different* '("pic1/debian.png" "pic1/debian_logo.png"))

(defun vs-check (ok label)
  (unless ok
    (incf *vs-fails*)
    (format t "FAIL: ~a~%" label))
  ok)

(defun vs-read (path)
  (with-open-file (s path :element-type '(unsigned-byte 8))
    (let ((v (make-array (file-length s) :element-type '(unsigned-byte 8))))
      (read-sequence v s)
      v)))

(defun vs-same (a b)
  (and a b (= (length a) (length b)) (every #'= a b)))

(defun vs-exfat-files (v dir first cont size acc)
  "(PATH BYTES CONTIGUOUS) for every file below the exFAT directory DIR."
  (dolist (e (fs-fat::%ex-scan-dir v first cont size) acc)
    (let ((path (if (string= dir "") (first e) (concatenate 'string dir "/" (first e)))))
      (if (second e)
          (setq acc (vs-exfat-files v path (fourth e) (fifth e) (third e) acc))
          (push (list path (exfat-read-file v path) (fifth e)) acc)))))

(defun vs-fat-files (v dir acc)
  (dolist (e (fat32-directory v dir) acc)
    (let ((path (if (string= dir "") (first e) (concatenate 'string dir "/" (first e)))))
      (if (second e)
          (setq acc (vs-fat-files v path acc))
          (push (list path (fat32-read-file v path)) acc)))))

(defun vs-write-export (files)
  (dolist (f files)
    (let ((path (concatenate 'string *vs-export* "/" (first f))))
      (ensure-directories-exist path)
      (with-open-file (s path :direction :output :element-type '(unsigned-byte 8)
                              :if-exists :supersede)
        (write-sequence (second f) s)))))

(defun vs-main ()
  (let* ((ex (exfat-open (vs-read *vs-exfat-part*)))
         (fv (fat32-open (vs-read *vs-fat-part*)))
         (exfiles (sort (vs-exfat-files ex "" (fs-fat::%ex-root ex) nil 0 nil) #'string< :key #'first))
         (fatfiles (sort (vs-fat-files fv "" nil) #'string< :key #'first)))
    (vs-check (= (length exfiles) 18) "exFAT volume has 18 files")
    (vs-check (= (length fatfiles) 18) "FAT volume has 18 files")
    (vs-check (equal (mapcar #'first exfiles) (mapcar #'first fatfiles))
              "both volumes list the same paths")
    (dolist (f exfiles)
      (let* ((path (first f))
             (other (find path fatfiles :key #'first :test #'string=)))
        (vs-check (vs-same (second f) (second other))
                  (format nil "exFAT and FAT agree on ~a" path))
        (vs-check (third f) (format nil "exFAT ~a is contiguous" path))
        (let* ((orig (vs-read (concatenate 'string *vs-original* "/" path)))
               (same (vs-same (second f) orig)))
          (if (member path *vs-known-different* :test #'string=)
              (vs-check (and (second f) (= (length (second f)) (length orig)) (not same))
                        (format nil "~a differs from its original, as known" path))
              (vs-check same (format nil "~a matches its original" path))))))
    (vs-write-export fatfiles)
    (format t "VOLUME-SAMPLES ~a failures=~a~%"
            (if (= *vs-fails* 0) "PASS" "FAIL") *vs-fails*)))

(vs-main)
