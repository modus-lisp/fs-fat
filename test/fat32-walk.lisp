;;;; test/fat32-walk.lisp -- walk a FAT32 image, optionally under byte patches.
;;;; Portable CL.  Loaded by test/fat32-oracle.lisp after lib/fat32.lisp.
;;;;
;;;; (WK-RUN BYTES PATCHES) applies PATCHES -- a list of (OFFSET . VALUE) -- to
;;;; BYTES in place, walks the volume, restores BYTES, and returns a plist:
;;;;
;;;;   :status   "ok" | "fat32-error" | "limit" | "internal"
;;;;   :msg      the condition's text ("" when ok)
;;;;   :geom     (bps spc fat-start data-start nclusters), or NIL if the open failed
;;;;   :entries  in walk order, with the root first:
;;;;               (:dir PATH CHAIN)
;;;;               (:file PATH FNV32 SIZE CHAIN)
;;;;             PATH is "" for the root and "a/b" below it; CHAIN is the list of
;;;;             clusters, empty for an empty file.
;;;;
;;;; STATUS: "fat32-error" is the reader's own diagnostic (its message starts
;;;; "fat32:").  "limit" is the walker's depth or entry budget stopping a loop
;;;; of directories.  "internal" is anything else: an unexpected condition, or
;;;; heap exhaustion.  An "internal" is a defect in the reader.

(defun wk-fnv (bytes)
  "32-bit FNV-1a of BYTES."
  (let ((h #x811C9DC5))
    (dotimes (i (length bytes))
      (setq h (logand (* (logxor h (aref bytes i)) #x01000193) #xFFFFFFFF)))
    h))

(defun wk-prefix-p (prefix s)
  (and (>= (length s) (length prefix))
       (string= prefix s :end2 (length prefix))))

(defun wk-status (msg)
  (cond ((wk-prefix-p "fat32:" msg) "fat32-error")
        ((wk-prefix-p "walk:" msg) "limit")
        (t "internal")))

(defun wk-emit (acc entry)
  (setf (car acc) (cons entry (car acc))))

(defun wk-walk (v cluster path depth acc budget)
  "Walk the directory at CLUSTER, displayed as PATH.  BUDGET is a one-element
   list counting directory entries down to zero."
  (when (> depth 12)
    (error "walk: directory depth limit reached at ~a" path))
  (dolist (e (fs-fat::%fat-dir-entries v cluster))
    (setf (car budget) (- (car budget) 1))
    (when (< (car budget) 0)
      (error "walk: entry budget exhausted"))
    (let* ((name (nth 0 e))
           (p (if (string= path "") name (concatenate 'string path "/" name)))
           (chain (fs-fat::%fat-chain v (nth 3 e))))
      (if (nth 1 e)
          (progn
            (wk-emit acc (list :dir p chain))
            (wk-walk v (nth 3 e) p (+ depth 1) acc budget))
          (let ((data (fs-fat::%fat-read-clusters v chain (nth 2 e))))
            (wk-emit acc (list :file p (wk-fnv data) (length data) chain)))))))

(defun wk-check-patches (bytes patches)
  (dolist (p patches)
    (when (or (< (car p) 0) (>= (car p) (length bytes)))
      (error "walk: patch offset ~a is outside the image" (car p)))))

(defun wk-apply (bytes patches)
  "Apply PATCHES in order.  Return the undo list, most recent change first."
  (let ((undo nil))
    (dolist (p patches)
      (setq undo (cons (cons (car p) (aref bytes (car p))) undo))
      (setf (aref bytes (car p)) (cdr p)))
    undo))

(defun wk-restore (bytes undo)
  (dolist (u undo)
    (setf (aref bytes (car u)) (cdr u))))

(defun wk-run (bytes patches)
  (let ((acc (list nil))
        (budget (list 4000))
        (undo nil)
        (status "ok")
        (msg "")
        (geom nil))
    (unwind-protect
         (handler-case
             (progn
               (wk-check-patches bytes patches)
               (setq undo (wk-apply bytes patches))
               (let* ((v (fat32-open bytes))
                      (root (fs-fat::%fat-root v)))
                 (setq geom (list (fs-fat::%fat-bps v) (fs-fat::%fat-spc v) (fs-fat::%fat-fat-start v)
                                  (fs-fat::%fat-data-start v) (fs-fat::%fat-nclusters v)))
                 (wk-emit acc (list :dir "" (fs-fat::%fat-chain v root)))
                 (wk-walk v root "" 0 acc budget)))
           (serious-condition (c)
             (setq msg (format nil "~a" c))
             (setq status (wk-status msg))))
      (wk-restore bytes undo))
    (list :status status :msg msg :geom geom :entries (reverse (car acc)))))
