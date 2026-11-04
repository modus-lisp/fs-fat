;;;; test/fat32-format-test.lisp -- FAT32-FORMAT on a sparse image file, through a block
;;;; device as the board's SD card is: the volume opens, takes files and directories, and
;;;; reads back after a reopen.  test/run-fat32-format.sh then has mtools (and fsck.fat,
;;;; where it is installed) read the image cold.
;;;;   sbcl --script test/fat32-format-test.lisp IMAGE BYTES
(require :asdf)
(load (merge-pathnames "../lib/fat32.lisp" *load-truename*))
(in-package #:fs-fat)
(defvar *fails* 0)
(defun check (name got want)
  (if (equalp got want) (format t "ok    ~a~%" name)
      (progn (incf *fails*) (format t "FAIL  ~a: got ~s want ~s~%" name got want))))
(defun %bytes (s) (map 'vector #'char-code s))
(let* ((path (second sb-ext:*posix-argv*))
       (size (parse-integer (third sb-ext:*posix-argv*)))
       (stream (open path :direction :io :element-type '(unsigned-byte 8)
                          :if-exists :overwrite :if-does-not-exist :create))
       (writes 0))
  (file-position stream (1- size)) (write-byte 0 stream)       ; sparse, SIZE bytes
  (flet ((dev ()
           (make-fat32-block-device
            size
            (lambda (off n) (let ((v (make-array n :element-type '(unsigned-byte 8))))
                              (file-position stream off) (read-sequence v stream) (coerce v 'vector)))
            (lambda (off src) (incf writes) (file-position stream off)
              (write-sequence (coerce src '(vector (unsigned-byte 8))) stream))
            (lambda () (finish-output stream)))))
    (let ((v (fat32-format (dev) :label "MODUS PKG" :volume-id #x4D4F4455 :hidden-sectors 10485760)))
      (format t "~&formatted: ~a sectors/cluster, ~a clusters, ~a FAT sectors, ~a sector writes~%"
              (%fat-spc v) (%fat-nclusters v) (floor (%fat-fatsz v) 512) writes)
      (check "empty root" (fat32-directory v "/") nil)
      (fat32-mkdir v "/pkgs")
      (fat32-write-file v "/pkgs/hello.lisp" (%bytes "(print :hello)"))
      (fat32-write-file v "/big.bin" (coerce (loop for i below 200000 collect (mod (* i 7) 256)) 'vector))
      (fat32-sync v))
    (let ((v (fat32-open-device (dev))))                    ; reopened from the image
      (check "files after reopen" (sort (mapcar #'first (fat32-directory v "/")) #'string<) '("big.bin" "pkgs"))
      (check "small file" (fat32-read-file v "/pkgs/hello.lisp") (%bytes "(print :hello)"))
      (check "big file" (fat32-read-file v "/big.bin")
             (coerce (loop for i below 200000 collect (mod (* i 7) 256)) 'vector))))
  (close stream))
(format t "~&fat32-format: ~a~%" (if (zerop *fails*) "PASS" "FAIL"))
(sb-ext:exit :code (if (zerop *fails*) 0 1))
