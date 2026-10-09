;;;; test/fat32-ops.lisp -- random operations on a volume made by mkfs.vfat, then
;;;; three views of the result compared.  User mode throughout: mtools does the
;;;; Linux-side work, with no mount and no root.
;;;;
;;;; Starting from an empty volume, each step is one random operation (mkdir,
;;;; create, overwrite, delete, deltree, rename), applied by mtools to the image
;;;; and to an in-memory model.  Every few steps the volume is checked:
;;;;   MODEL   the in-memory tree the operations were applied to (ground truth)
;;;;   READER  lib/fat32.lisp walking the image
;;;;   MTOOLS  mtools exporting the image to the host, read back as files
;;;; and fsck.fat -n must accept the image.  A disagreement names the observer.
;;;;
;;;; Runner: test/run-fat32-ops.sh.  It defines these before loading this file:
;;;;   *oracle-root*  *oracle-workdir*  *ops-first-seed*  *ops-seeds*
;;;;   *ops-count*  *ops-every*
;;;; The helpers come from test/fat32-oracle.lisp, loaded here as a library.

(defvar *oracle-library-only* t)
(load (concatenate 'string *oracle-root* "test/fat32-oracle.lisp"))
(use-package :fs-fat)
(load (concatenate 'string *oracle-root* "test/fat32-hostdev.lisp"))

(defvar *mmd* "mmd")
(defvar *mren* "mren")
(defvar *mdeltree* "mdeltree")
(defvar *find* "find")

(defvar *ops-mtools-aborts* 0
  "Renames mtools 4.0.32 aborted on this seed.  Each one was checked to leave the
   volume unchanged; see test/mtools-mren-abort.sh for the minimal reproducer.")

(defvar *ops-budget* (* 12 1024 1024)
  "Stop creating and growing files once the model holds this many bytes.")

;;; ---- the model ------------------------------------------------------------------
;;; A model entry is (REL KIND BYTES), as in fat32-oracle.lisp.

(defun base-name (rel)
  (let ((slash (position #\/ rel :from-end t)))
    (if slash (subseq rel (+ slash 1)) rel)))

(defun dirs-of (model)
  (cons "" (loop for e in model when (eq (second e) :dir) collect (first e))))

(defun files-of (model)
  (loop for e in model when (eq (second e) :file) collect (first e)))

(defun children-names (model dir)
  "Upcased base names of DIR's children, for RAND-NAME's TAKEN list."
  (loop for e in model
        when (string= (parent-path (first e)) dir)
          collect (string-upcase (base-name (first e)))))

(defun total-bytes (model)
  (loop for e in model when (third e) sum (length (third e))))

(defun under-p (rel prefix)
  "True when REL is PREFIX itself or lies below it."
  (or (string= rel prefix)
      (wk-prefix-p (concatenate 'string prefix "/") rel)))

(defun model-add (model rel kind bytes)
  (cons (list rel kind bytes) model))

(defun model-replace-bytes (model rel bytes)
  (mapcar (lambda (e) (if (string= (first e) rel) (list rel :file bytes) e)) model))

(defun model-delete (model rel)
  (remove-if (lambda (e) (under-p (first e) rel)) model))

(defun model-rename (model old new)
  (mapcar (lambda (e)
            (let ((p (first e)))
              (if (under-p p old)
                  (list (concatenate 'string new (subseq p (length old))) (second e) (third e))
                  e)))
          model))

;;; ---- operations -------------------------------------------------------------------
;;; Each returns (NEW-MODEL DESCRIPTION), or NIL when it does not apply.  The
;;; mtools call has already happened by the time it returns, so a NIL means "no
;;; operation was attempted", never "the model became empty".

(defun incoming (src) (concatenate 'string src "/incoming.bin"))

(defun op-size (r cs)
  (let ((x (rng-real r)))
    (cond ((< x 0.10) 0)
          ((< x 0.40) (rng-range r 1 2000))
          ((< x 0.80) (rng-range r 2001 100000))
          (t (* cs (rng-range r 1 6))))))

(defun lib-apply (img fn)
  "Apply FN to the volume in IMG through lib/fat32.lisp, on a file device: each write
   reaches the image as it is made, and each flush syncs it."
  (file-device-volume img fn))

(defun lib-deltree (v path)
  "Remove PATH and everything under it, through the library."
  (dolist (e (fat32-directory v path))
    (let ((child (join-rel path (first e))))
      (if (second e)
          (lib-deltree v child)
          (fat32-delete v child))))
  (fat32-delete v path))

(defun by-lib-p (r)
  "Whether this operation is applied by lib/fat32.lisp rather than mtools.  Both
   write the same image, so every check sees whichever wrote last."
  (rng-chance r 0.5))

(defun op-mkdir (r img model)
  (let* ((dir (rng-pick r (dirs-of model)))
         (rel (join-rel dir (rand-name r (children-names model dir))))
         (lib (by-lib-p r)))
    (if lib
        (lib-apply img (lambda (v) (fat32-mkdir v rel)))
        (host-must *mmd* "-i" img (concatenate 'string "::" rel)))
    (list (model-add model rel :dir nil)
          (format nil "mkdir ~a~a" rel (if lib " [lib]" "")))))

(defun op-create (r img src model cs)
  (when (< (total-bytes model) *ops-budget*)
    (let* ((dir (rng-pick r (dirs-of model)))
           (rel (join-rel dir (rand-name r (children-names model dir))))
           (bytes (rand-bytes r (op-size r cs)))
           (lib (by-lib-p r)))
      (if lib
          (lib-apply img (lambda (v) (fat32-write-file v rel bytes)))
          (progn
            (write-bytes-file (incoming src) bytes)
            (host-must *mcopy* "-i" img (incoming src) (concatenate 'string "::" rel))))
      (list (model-add model rel :file bytes)
            (format nil "create ~a (~a bytes)~a" rel (length bytes) (if lib " [lib]" ""))))))

(defun op-overwrite (r img src model cs)
  (let ((files (files-of model)))
    (when (and files (< (total-bytes model) *ops-budget*))
      (let* ((rel (rng-pick r files))
             (bytes (rand-bytes r (op-size r cs)))
             (lib (by-lib-p r)))
        (if lib
            (lib-apply img (lambda (v) (fat32-write-file v rel bytes)))
            (progn
              (write-bytes-file (incoming src) bytes)
              (host-must *mcopy* "-D" "o" "-i" img (incoming src)
                         (concatenate 'string "::" rel))))
        (list (model-replace-bytes model rel bytes)
              (format nil "overwrite ~a (~a bytes)~a" rel (length bytes) (if lib " [lib]" "")))))))

(defun op-delete (r img model)
  (let ((files (files-of model)))
    (when files
      (let* ((rel (rng-pick r files))
             (lib (by-lib-p r)))
        (if lib
            (lib-apply img (lambda (v) (fat32-delete v rel)))
            (host-must *mdel* "-i" img (concatenate 'string "::" rel)))
        (list (model-delete model rel)
              (format nil "delete ~a~a" rel (if lib " [lib]" "")))))))

(defun op-deltree (r img model)
  (let ((dirs (remove "" (dirs-of model) :test #'string=)))
    (when dirs
      (let* ((rel (rng-pick r dirs))
             (lib (by-lib-p r)))
        (if lib
            (lib-apply img (lambda (v) (lib-deltree v rel)))
            (host-must *mdeltree* "-i" img (concatenate 'string "::" rel)))
        (list (model-delete model rel)
              (format nil "deltree ~a~a" rel (if lib " [lib]" "")))))))

(defun op-rename (r img src model)
  "Rename an entry.  Through the library, the new name may be in another directory;
   mtools' mren only renames within a directory."
  (when model
    (let* ((e (rng-pick r model))
           (old (first e))
           (lib (by-lib-p r))
           (parent (if (and lib (rng-chance r 0.5))
                       (rng-pick r (remove-if (lambda (d) (under-p d old)) (dirs-of model)))
                       (parent-path old)))
           (new (join-rel parent (rand-name r (children-names model parent)))))
      (if lib
          (progn
            (lib-apply img (lambda (v) (fat32-rename v old new)))
            (list (model-rename model old new)
                  (format nil "rename ~a -> ~a [lib]" old new)))
          (let ((argv (list "-i" img (concatenate 'string "::" old)
                            (concatenate 'string "::" new))))
            (multiple-value-bind (code out) (apply #'host-run *mren* argv)
              (cond ((= code 0)
                     (list (model-rename model old new)
                           (format nil "rename ~a -> ~a" old new)))
                    ;; SBCL reports a child killed by a signal as the signal number,
                    ;; so SIGABRT (6) shows up here as 6, not 134.
                    ((and (or (= code 6) (= code 134)) (search "dirCache.c" out))
                     ;; The known mtools abort.  It must leave the volume as it was:
                     ;; check that, then skip this rename.
                     (let ((problems (check-state img src model)))
                       (when problems
                         (error "mren aborted and changed the volume: ~a" (first problems)))
                       (incf *ops-mtools-aborts*)
                       nil))
                    (t (error "mren failed (exit ~a): ~a" code out)))))))))

(defun random-op (r img src model cs)
  "Apply one random operation that applies.  Returns (values NEW-MODEL DESCRIPTION)."
  (block op
    (dotimes (attempt 50)
      (let ((x (rng-int r 100)))
        (let ((res (cond ((< x 15) (op-mkdir r img model))
                         ((< x 45) (op-create r img src model cs))
                         ((< x 60) (op-overwrite r img src model cs))
                         ((< x 75) (op-delete r img model))
                         ((< x 82) (op-deltree r img model))
                         (t (op-rename r img src model)))))
          (when res
            (return-from op (values (first res) (second res)))))))
    (error "no operation applied in 50 attempts")))

;;; ---- the three views ---------------------------------------------------------------

(defun split-lines (s)
  (let ((out nil) (start 0))
    (loop for i from 0 below (length s)
          do (when (char= (char s i) #\Newline)
               (push (subseq s start i) out)
               (setq start (+ i 1))))
    (when (< start (length s))
      (push (subseq s start) out))
    (nreverse out)))

(defun walk-image (img)
  "The reader's view: a walk-shaped plist."
  (wk-run (read-image img) nil))

(defun export-image (img src)
  "mtools' view: the whole image exported to the host and read back, as a
   walk-shaped plist.  Files are read from the exported copy, so their bytes are
   what mtools actually wrote out."
  (let ((out (concatenate 'string src "/export")))
    (host-must *rm* "-rf" out)
    (host-must *mkdir* "-p" out)
    (host-must *mcopy* "-s" "-i" img "::" out)
    (let ((entries (list (list :dir "" nil))))
      (dolist (line (split-lines (host-must *find* out "-mindepth" "1" "-printf" "%y %P\\n")))
        (when (> (length line) 2)
          (let ((rel (subseq line 2)))
            (if (char= (char line 0) #\d)
                (push (list :dir rel nil) entries)
                (let ((data (read-image (host-path out rel))))
                  (push (list :file rel (wk-fnv data) (length data) nil) entries))))))
      (list :status "ok" :msg "" :entries (reverse entries)))))

(defun check-state (img src model)
  "Problems, as strings, where fsck, the reader or mtools disagree with MODEL."
  (let ((problems nil)
        (fsck (host-run *fsck* "-n" img)))
    (unless (= fsck 0)
      (push (format nil "fsck.fat -n rejected the image (exit ~a)" fsck) problems))
    (setq problems (append problems (check-tree model (walk-image img) "reader" t)))
    (setq problems (append problems (check-tree model (export-image img src) "mtools" t)))
    problems))

;;; ---- driver -------------------------------------------------------------------------

(defun run-ops-seed (seed nops every workdir)
  "Run one seed.  Returns T when every check agrees."
  (let* ((r (rng-make seed))
         (spc (rng-pick r '(1 2)))
         (cs (* 512 spc))
         (dir (concatenate 'string workdir "/ops-" (princ-to-string seed)))
         (src (concatenate 'string dir "/src"))
         (img (concatenate 'string dir "/vol.img"))
         (model nil)
         (log nil))
    (setq *ops-mtools-aborts* 0)
    (host-must *mkdir* "-p" src)
    (host-must *mkfs* "-F" "32" "-s" (princ-to-string spc) "-n" "OPSVOL" "-C" img
               (princ-to-string (if (= spc 1) 40960 70000)))
    (let ((step 0))
      (flet ((report-recent ()
               (format t "  most recent operations:~%")
               (dolist (l (subseq log 0 (min 8 (length log))))
                 (format t "    ~a~%" l))))
        (handler-case
            (dotimes (i nops)
              (setq step (+ i 1))
              (multiple-value-bind (m desc) (random-op r img src model cs)
                (setq model m)
                (push (format nil "~a: ~a" (+ i 1) desc) log))
              (when (or (zerop (mod (+ i 1) every)) (= (+ i 1) nops))
                (let ((problems (check-state img src model)))
                  (when problems
                    (format t "seed ~a: FAIL after operation ~a~%" seed (+ i 1))
                    (dolist (p (subseq problems 0 (min 10 (length problems))))
                      (format t "  ~a~%" p))
                    (report-recent)
                    (return-from run-ops-seed nil)))))
          ;; A host tool that aborts is itself a finding: report the operation
          ;; and keep the image so the case can be replayed.
          (error (e)
            (format t "seed ~a: a host tool failed at operation ~a: ~a~%" seed step e)
            (report-recent)
            (format t "  image kept at ~a~%" img)
            (return-from run-ops-seed nil)))))
    (format t "seed ~a: spc=~a ops=~a files=~a dirs=~a -- reader, mtools and model agree~%"
            seed spc nops
            (count :file model :key #'second)
            (count :dir model :key #'second))
    (format t "  applied by lib/fat32.lisp: ~a of ~a operations~%"
            (count-if (lambda (d) (search "[lib]" d)) log) nops)
    (when (> *ops-mtools-aborts* 0)
      (format t "  mtools aborted ~a rename(s) (known bug; volume unchanged each time)~%"
              *ops-mtools-aborts*))
    t))

(defun ops-main ()
  (let ((ok t))
    (dotimes (k *ops-seeds*)
      (unless (run-ops-seed (+ *ops-first-seed* k) *ops-count* *ops-every* *oracle-workdir*)
        (setq ok nil)))
    (format t "FAT32-OPS ~a~%" (if ok "PASS" "FAIL"))))

(ops-main)
