;;;; test/fat32-hostdev.lisp -- a FAT32 device over an image file, for the tests.
;;;; Host side: the flush calls sync(1) through HOST-RUN, which fat32-oracle.lisp
;;;; defines.  Everything else is portable CL.
;;;;
;;;; The view is the file's contents, read in when the device is opened.  Each sector
;;;; write also goes to the file, and each flush syncs the file to disk.  So the file
;;;; always holds what the library has written, and a crash leaves it as it was at
;;;; the crash.

(defun open-file-device (path)
  "(values DEVICE STREAM) for the image at PATH.  Close STREAM when done."
  (let* ((stream (open path :direction :io :element-type '(unsigned-byte 8)
                            :if-exists :overwrite))
         (view (make-array (file-length stream) :element-type '(unsigned-byte 8))))
    (read-sequence view stream)
    (values (make-fat32-device
             view
             (lambda (off src)
               (file-position stream off)
               (write-sequence src stream))
             (lambda ()
               (finish-output stream)
               (host-must "sync" "-f" path)))
            stream)))

(defun file-device-volume (path fn)
  "Open the image at PATH as a device, call FN on the volume, sync, and close.
   Returns what FN returns."
  (multiple-value-bind (dev stream) (open-file-device path)
    (unwind-protect
         (let* ((v (fat32-open-device dev))
                (result (funcall fn v)))
           (fat32-sync v)
           result)
      (close stream))))
