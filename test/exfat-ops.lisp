;;;; test/exfat-ops.lisp -- random operations on an exFAT volume, checked against an
;;;; in-memory model after every few operations, and by fsck.exfat -n.
;;;; Portable CL except HOST-RUN (from fat32-oracle.lisp), used to run fsck.exfat.
;;;;
;;;; The runner (test/run-exfat-ops.sh) defines before loading this file:
;;;;   *oracle-root* *oracle-workdir* *ops-first-seed* *ops-seeds* *ops-count* *ops-every*
;;;; and makes the volume with mkfs.exfat (MKFS_EXFAT, default mkfs.exfat).

(defvar *oracle-library-only* t)
(load (concatenate 'string *oracle-root* "test/fat32-oracle.lisp"))
(use-package :fs-fat)
(load (concatenate 'string *oracle-root* "lib/exfat.lisp"))

(defvar *fsck-exfat* "fsck.exfat")
(defvar *mkfs-exfat* "mkfs.exfat")

;;; ---- the model: (REL KIND BYTES), as elsewhere ----------------------------------

(defun xo-parent (rel)
  (let ((slash (position #\/ rel :from-end t)))
    (if slash (subseq rel 0 slash) "")))

(defun xo-base (rel)
  (let ((slash (position #\/ rel :from-end t)))
    (if slash (subseq rel (+ slash 1)) rel)))

(defun xo-dirs (model)
  (cons "" (loop for e in model when (eq (second e) :dir) collect (first e))))

(defun xo-files (model)
  (loop for e in model when (eq (second e) :file) collect (first e)))

(defun xo-children-names (model dir)
  (loop for e in model
        when (string= (xo-parent (first e)) dir)
          collect (string-upcase (xo-base (first e)))))

(defun xo-under (rel prefix)
  (or (string= rel prefix)
      (and (> (length rel) (length prefix))
           (string= prefix rel :end2 (length prefix))
           (char= (char rel (length prefix)) #\/))))

(defun xo-rename-model (model old new)
  (mapcar (lambda (e)
            (if (xo-under (first e) old)
                (list (concatenate 'string new (subseq (first e) (length old)))
                      (second e) (third e))
                e))
          model))

(defun xo-total-bytes (model)
  (loop for e in model when (third e) sum (length (third e))))

;;; ---- operations: each returns (NEW-MODEL DESCRIPTION) or NIL -------------------------

(defun xo-op-mkdir (r v model)
  (let* ((dir (rng-pick r (xo-dirs model)))
         (rel (join-rel dir (rand-name r (xo-children-names model dir)))))
    (exfat-mkdir v rel)
    (list (cons (list rel :dir nil) model) (format nil "mkdir ~a" rel))))

(defun xo-op-write (r v model)
  (let* ((create (or (null (xo-files model)) (rng-chance r 0.5)))
         (size (rng-pick r (list 0 1 100 700 4000 9000 40000)))
         (bytes (rand-bytes r size)))
    (if create
        (let* ((dir (rng-pick r (xo-dirs model)))
               (rel (join-rel dir (rand-name r (xo-children-names model dir)))))
          (exfat-write-file v rel bytes)
          (list (cons (list rel :file bytes) model)
                (format nil "create ~a (~a bytes)" rel size)))
        (let ((rel (rng-pick r (xo-files model))))
          (exfat-write-file v rel bytes)
          (list (mapcar (lambda (e) (if (string= (first e) rel) (list rel :file bytes) e)) model)
                (format nil "overwrite ~a (~a bytes)" rel size))))))

(defun xo-op-delete (r v model)
  (let ((files (xo-files model)))
    (when files
      (let ((rel (rng-pick r files)))
        (exfat-delete v rel)
        (list (remove-if (lambda (e) (string= (first e) rel)) model)
              (format nil "delete ~a" rel))))))

(defun xo-op-rename (r v model)
  (when model
    (let* ((e (rng-pick r model))
           (old (first e))
           (dir (rng-pick r (remove-if (lambda (d) (xo-under d old)) (xo-dirs model))))
           (new (join-rel dir (rand-name r (xo-children-names model dir)))))
      (exfat-rename v old new)
      (list (xo-rename-model model old new) (format nil "rename ~a -> ~a" old new)))))

(defun xo-random-op (r v model)
  (block op
    (dotimes (attempt 50 (error "exfat ops: no operation applied"))
      (let* ((x (rng-int r 100))
             (res (cond ((< x 15) (xo-op-mkdir r v model))
                        ((< x 60) (xo-op-write r v model))
                        ((< x 75) (xo-op-delete r v model))
                        (t (xo-op-rename r v model)))))
        (when res
          (return-from op (values (first res) (second res))))))))

;;; ---- checks ----------------------------------------------------------------------------

(defun xo-check (bytes model)
  "Problems, as strings: the reader against the model."
  (let ((v (exfat-open bytes))
        (problems nil))
    (dolist (rel (xo-files model))
      (let* ((want (third (find rel model :key #'first :test #'string=)))
             (got (exfat-read-file v rel)))
        (unless (and got (= (length got) (length want)) (every #'= got want))
          (push (format nil "contents of ~s" rel) problems))))
    (dolist (d (xo-dirs model))
      (let* ((listed (mapcar #'first (exfat-directory v d)))
             (want (mapcar #'xo-base
                           (loop for e in model
                                 when (string= (xo-parent (first e)) d)
                                   collect (first e)))))
        (unless (and (= (length listed) (length want))
                     (every (lambda (w) (member w listed :test #'string=)) want))
          (push (format nil "listing of ~s: got ~s want ~s" d listed want) problems))))
    problems))

(defun xo-fsck (img)
  (let ((code (host-run *fsck-exfat* "-n" img)))
    (if (= code 0) nil (list (format nil "fsck.exfat -n exit ~a" code)))))

;;; ---- driver -----------------------------------------------------------------------------

(defun xo-run-seed (seed nops every workdir)
  (let* ((r (rng-make seed))
         (img (concatenate 'string workdir "/exfat-" (princ-to-string seed) ".img"))
         (model nil)
         (log nil))
    (host-must "truncate" "-s" "64M" img)
    (host-must *mkfs-exfat* "-n" "OPS" img)
    (let ((v (exfat-open (read-image img))))
      (dotimes (i nops)
        (multiple-value-bind (m desc) (xo-random-op r v model)
          (setq model m)
          (push (format nil "~a: ~a" (+ i 1) desc) log)
          (when (or (zerop (mod (+ i 1) every)) (= (+ i 1) nops))
            (write-bytes-file img (fs-fat::%ex-bytes v))
            (let ((problems (append (xo-fsck img) (xo-check (read-image img) model))))
              (when problems
                (format t "seed ~a: FAIL after operation ~a~%" seed (+ i 1))
                (dolist (p (subseq problems 0 (min 8 (length problems))))
                  (format t "  ~a~%" p))
                (format t "  most recent operations:~%")
                (dolist (l (subseq log 0 (min 6 (length log))))
                  (format t "    ~a~%" l))
                (return-from xo-run-seed nil)))))))
    (format t "seed ~a: ~a operations, ~a files, ~a dirs -- reader and fsck agree~%"
            seed nops (count :file model :key #'second) (count :dir model :key #'second))
    t))

(defun xo-main ()
  (let ((ok t))
    (dotimes (k *ops-seeds*)
      (unless (xo-run-seed (+ *ops-first-seed* k) *ops-count* *ops-every* *oracle-workdir*)
        (setq ok nil)))
    (format t "EXFAT-OPS ~a~%" (if ok "PASS" "FAIL"))))

(xo-main)
