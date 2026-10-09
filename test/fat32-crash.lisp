;;;; test/fat32-crash.lisp -- crash consistency of lib/fat32.lisp.
;;;;
;;;; A scenario is a sequence of operations, run in order on one volume, each synced
;;;; at its end.  The disk dies at event K (the Kth write or flush over the whole
;;;; sequence), so the library is interrupted at exactly that point.  Two passes:
;;;;
;;;;   MEMORY: a simulated disk (lib/fat32.lisp, the devices section).  What a crash
;;;;     leaves is every write before the last flush, plus a chosen subset of the
;;;;     writes after it: none, all, or random subsets, since a disk may reorder
;;;;     unflushed writes.
;;;;   FILE: a real image file (test/fat32-hostdev.lisp).  Writes reach the file as
;;;;     they are made, so the file is what the library has written up to the crash.
;;;;
;;;; Each crash image is repaired with fsck.fat -a, must then pass fsck.fat -n, and
;;;; must meet the contract of the operation the crash interrupted:
;;;;
;;;;   every operation before it is durable, so the state before the interrupted
;;;;     operation is the state after the one before it;
;;;;   a single path is in its state before the operation or its state after it;
;;;;   a rename is visible under the old name or the new name; if under neither, the
;;;;     data survives in a recovery file, which fsck.fat -a writes from clusters no
;;;;     entry refers to;
;;;;   a directory moved to another parent keeps its contents under each name it is
;;;;     visible under;
;;;;   every path the operation does not touch is unchanged.
;;;;
;;;; The memory pass is a model of crashes, not a power cut.  The file pass models a
;;;; process dying, which keeps what the kernel has been given.  Both assume writes
;;;; are atomic at sector size, as the library assumes.
;;;;
;;;; Runner: test/run-fat32-crash.sh.  It defines before loading this file:
;;;;   *oracle-root*  *oracle-workdir*  *crash-random-trials*

(defvar *oracle-library-only* t)
(load (concatenate 'string *oracle-root* "test/fat32-oracle.lisp"))
(use-package :fs-fat)
(load (concatenate 'string *oracle-root* "test/fat32-hostdev.lisp"))

(defun crash-pat (n seed)
  (let ((v (make-array n :element-type '(unsigned-byte 8))))
    (dotimes (i n) (setf (aref v i) (mod (+ (* i 31) seed) 251)))
    v))

(defun crash-base-paths ()
  '("old.txt" "keep.txt" "d" "e" "d/sub" "d/keepme.bin" "d/sub/inner.txt" "e/empty"))

(defun crash-setup (v)
  (fat32-mkdir v "d")
  (fat32-mkdir v "e")
  (fat32-mkdir v "d/sub")
  (fat32-mkdir v "e/empty")
  (fat32-write-file v "old.txt" (crash-pat 1500 1))
  (fat32-write-file v "keep.txt" (crash-pat 900 2))
  (fat32-write-file v "d/keepme.bin" (crash-pat 2000 3))
  (fat32-write-file v "d/sub/inner.txt" (crash-pat 700 4))
  (fat32-sync v))

;;; ---- operations ----------------------------------------------------------------------
;;; An operation is a plist: :NAME, :OP (a function of the volume), and either :SINGLE
;;; (a path) or :RENAME (old new).  :CHILD T says a directory moved under a new parent
;;; holds d/sub/inner.txt; :ALSO lists further paths the operation touches.

(defun crash-op (name &key single rename child also op)
  (list :name name :single single :rename rename :child child :also also :op op))

(defun crash-touched (op)
  (let ((touch (if (getf op :single)
                   (list (getf op :single))
                   (copy-list (getf op :rename)))))
    (when (getf op :child)
      (push "d/sub/inner.txt" touch))
    (append (getf op :also) touch)))

(defun crash-write (name path n seed)
  (crash-op name :single path :op (lambda (v) (fat32-write-file v path (crash-pat n seed)))))

(defun crash-delete (name path)
  (crash-op name :single path :op (lambda (v) (fat32-delete v path))))

(defun crash-rename (name old new &key child also)
  (crash-op name :rename (list old new) :child child :also also
            :op (lambda (v) (fat32-rename v old new))))

(defun crash-mkdir (name path)
  (crash-op name :single path :op (lambda (v) (fat32-mkdir v path))))

(defun crash-scenarios ()
  "Single operations, each on the base volume, and sequences of operations."
  (list
   (list :name "create new.txt" :ops (list (crash-write "create new.txt" "new.txt" 1200 5)))
   (list :name "create a long-named file"
         :ops (list (crash-write "create long" "a long name for a brand new file.txt" 600 6)))
   (list :name "replace old.txt, larger" :ops (list (crash-write "replace" "old.txt" 3000 7)))
   (list :name "replace old.txt, smaller" :ops (list (crash-write "replace" "old.txt" 100 8)))
   (list :name "mkdir newdir" :ops (list (crash-mkdir "mkdir" "newdir")))
   (list :name "delete old.txt" :ops (list (crash-delete "delete" "old.txt")))
   (list :name "delete empty directory" :ops (list (crash-delete "rmdir" "e/empty")))
   (list :name "rename old.txt" :ops (list (crash-rename "rename" "old.txt" "moved.txt")))
   (list :name "rename old.txt to a long name"
         :ops (list (crash-rename "rename" "old.txt"
                                  "a much longer destination name for the file.txt")))
   (list :name "move directory d/sub under e"
         :ops (list (crash-rename "move" "d/sub" "e/sub" :child t)))
   (list :name "sequence: create, replace, delete"
         :ops (list (crash-write "create" "new.txt" 1200 5)
                    (crash-write "replace" "new.txt" 3000 9)
                    (crash-delete "delete" "new.txt")))
   (list :name "sequence: rename, then write under the new name"
         :ops (list (crash-rename "rename" "old.txt" "moved.txt")
                    (crash-write "replace moved.txt" "moved.txt" 2500 10)))
   (list :name "sequence: make a directory, fill it, move it"
         :ops (list (crash-mkdir "mkdir" "x")
                    (crash-write "write x/f.bin" "x/f.bin" 1500 11)
                    (crash-rename "move x" "x" "e/x" :also '("x/f.bin" "e/x/f.bin"))))
   (list :name "sequence: write, delete, then reuse the freed clusters"
         :ops (list (crash-write "write a.bin" "a.bin" 5000 12)
                    (crash-write "write b.bin" "b.bin" 5000 13)
                    (crash-delete "delete a.bin" "a.bin")
                    (crash-write "write c.bin over the freed clusters" "c.bin" 9000 14)))
   (list :name "sequence: move a file out, then remove its directory"
         :ops (list (crash-rename "move inner.txt" "d/sub/inner.txt" "e/inner.txt")
                    (crash-delete "rmdir d/sub" "d/sub")))))

;;; ---- the device and the image --------------------------------------------------------

(defun run-sequence (dev ops &optional after-op)
  "Run OPS in order on DEV, syncing after each.  Returns the event count before each
   operation, and after the last: N+1 numbers for N operations.  AFTER-OP, if given,
   is called with the view after each operation."
  (let ((v (fat32-open-device dev))
        (bounds nil))
    (dolist (op ops)
      (push (device-events dev) bounds)
      (handler-case (progn (funcall (getf op :op) v) (fat32-sync v))
        (error (e)
          (unless (device-dead-p dev)
            (error "operation ~a failed on a live device: ~a" (getf op :name) e))))
      (when after-op
        (funcall after-op (svref dev 0))))
    (push (device-events dev) bounds)
    (reverse bounds)))

(defun op-at-event (bounds k)
  "The operation that event K belongs to, given the event counts before each."
  (let ((i 0))
    (loop while (<= (nth (+ i 1) bounds) k)
          do (incf i))
    i))

(defun snapshot (view paths)
  (let ((v (fat32-open view)))
    (mapcar (lambda (p) (cons p (path-state v p))) paths)))

(defun crash-keeps (k)
  "Which unflushed writes survive a crash at point K: none, all, and some random
   subsets, each a function of the write's position."
  (append (list :none :all)
          (loop for r from 1 to *crash-random-trials*
                collect (let ((rng (rng-make (+ 1000 (* 97 k) (* 13 r)))))
                          (lambda (i) (declare (ignore i)) (rng-chance rng 0.5))))))

(defun keep-name (keep)
  (cond ((eq keep :none) "none persisted")
        ((eq keep :all) "all persisted")
        (t "random subset persisted")))

(defun apply-writes (path writes)
  (with-open-file (s path :direction :io :element-type '(unsigned-byte 8)
                          :if-exists :overwrite)
    (dolist (w writes)
      (file-position s (car w))
      (write-sequence (cdr w) s))))

(defun crash-check-image (img base-file writes op base-states)
  "Build the crash image from the writes that survive, then check it."
  (host-must "cp" "--sparse=always" base-file img)
  (apply-writes img writes)
  (crash-check-file img op base-states))

(defun path-state (v p)
  "What is at P: NIL, (:DIR), or (:FILE FNV32 SIZE)."
  (let ((e (fat32-lookup v p)))
    (cond ((null e) nil)
          ((nth 1 e) (list :dir))
          (t (let ((d (fat32-read-file v p)))
               (list :file (wk-fnv d) (length d)))))))

(defun crash-recovered-p (v before)
  "Whether the data that was at a renamed name survived as a recovery file in the
   root.  fsck.fat -a writes clusters no entry refers to into FSCK*.REC (or FILE*.CHK
   in older versions), padded to whole clusters.  For a file, the original bytes must
   be the prefix of a recovery file; for a directory, any recovery file will do."
  (some (lambda (e)
          (let ((n (first e)))
            (and (not (second e))
                 (> (length n) 8)
                 (or (string-equal "FSCK" n :end2 4) (string-equal "FILE" n :end2 4))
                 (or (string-equal ".REC" n :start2 (- (length n) 4))
                     (string-equal ".CHK" n :start2 (- (length n) 4)))
                 (or (not (eq (first before) :file))
                     (let ((d (fat32-read-file v n)))
                       (and (>= (length d) (third before))
                            (= (wk-fnv (subseq d 0 (third before))) (second before))))))))
        (fat32-directory v "")))

(defun crash-check-file (img op base-states)
  "Repair the crash image at IMG, and check it against OP.  BASE-STATES gives the
   state of each path before OP.  Returns a failure message, or NIL."
  (let ((fixed (host-run *fsck* "-a" img)))
    (unless (<= fixed 1)
      (return-from crash-check-file (format nil "fsck -a exited ~a" fixed))))
  (unless (= (host-run *fsck* "-n" img) 0)
    (return-from crash-check-file "not clean after repair (fsck -n)"))
  (let ((v (handler-case (fat32-open (read-image img))
             (error (e) (return-from crash-check-file
                          (format nil "reopen after repair failed: ~a" e))))))
    (handler-case
        (progn
          (dolist (p (mapcar #'car base-states))
            (unless (member p (crash-touched op) :test #'string=)
              (let ((now (path-state v p))
                    (was (cdr (assoc p base-states :test #'string=))))
                (unless (equal now was)
                  (return-from crash-check-file
                    (format nil "untouched ~s changed: ~s -> ~s" p was now))))))
          (cond
            ((getf op :single)
             (let* ((p (getf op :single))
                    (before (cdr (assoc p base-states :test #'string=)))
                    (now (path-state v p)))
               (unless (or (equal now before) (equal now (getf op :after)))
                 (return-from crash-check-file
                   (format nil "~s is ~s, expected ~s or ~s" p now before (getf op :after))))))
            (t
             (destructuring-bind (old new) (getf op :rename)
               (let* ((before (cdr (assoc old base-states :test #'string=)))
                      (a (path-state v old))
                      (b (path-state v new)))
                 (unless (or (and (equal a before) (null b))
                             (and (null a) (equal b before))
                             (and (equal a before) (equal b before))
                             (and (null a) (null b) (crash-recovered-p v before)))
                   (return-from crash-check-file
                     (format nil "rename ~s -> ~s left ~s and ~s" old new a b)))
                 (when (getf op :child)
                   (let ((inner (cdr (assoc "d/sub/inner.txt" base-states :test #'string=))))
                     (dolist (loc (list old new))
                       (when (equal (path-state v loc) '(:dir))
                         (let ((child (path-state v (concatenate 'string loc "/inner.txt"))))
                           (unless (equal child inner)
                             (return-from crash-check-file
                               (format nil "moved directory ~s lost its contents: ~s"
                                       loc child)))))))))))))
      (error (e)
        (format nil "lookup failed after repair: ~a" e)))))

;;; ---- scenarios ----------------------------------------------------------------------

(defun run-crash-scenario (sc base base-file img)
  "Crash the scenario's sequence at every event, and check each crash image.  Returns
   (values FAILURES POINTS IMAGES)."
  (let* ((ops (getf sc :ops))
         (n (length ops))
         (universe (remove-duplicates
                    (append (crash-base-paths) (loop for op in ops append (crash-touched op)))
                    :test #'string=))
         (snaps nil)
         (bounds (run-sequence (make-fat32-device (copy-seq base)) ops
                               (lambda (view) (push (snapshot view universe) snaps))))
         (states (cons (snapshot base universe) (reverse snaps)))
         (total (car (last bounds)))
         (failures nil)
         (images 0))
    (dotimes (k (+ total 1))
      ;; Crash point K interrupts operation I.  Past the last event, the sequence has
      ;; completed, and the contract is the last operation's.
      (let* ((i (if (< k total) (op-at-event bounds k) (- n 1)))
             (op (nth i ops))
             (target (or (getf op :single) (second (getf op :rename))))
             (before (nth i states))
             (opx (append op (list :after (cdr (assoc target (nth (+ i 1) states)
                                                      :test #'string=)))))
             (where (format nil "crash at event ~a of ~a, in op ~a (~a)"
                            k total (+ i 1) (getf op :name))))
        ;; memory pass: one run, then each way the unflushed writes may have landed
        (let ((dev (make-fat32-device (copy-seq base))))
          (setf (svref dev 4) (if (< k total) k nil))
          (run-sequence dev ops)
          (dolist (keep (crash-keeps k))
            (let ((msg (crash-check-image img base-file (device-crash-writes dev keep)
                                          opx before)))
              (incf images)
              (when msg
                (push (format nil "memory, ~a, ~a: ~a" (keep-name keep) where msg)
                      failures)))))
        ;; file pass: the image file is what the library wrote before the crash
        (host-must "cp" "--sparse=always" base-file img)
        (multiple-value-bind (dev stream) (open-file-device img)
          (setf (svref dev 4) (if (< k total) k nil))
          (unwind-protect (run-sequence dev ops)
            (close stream)))
        (let ((msg (crash-check-file img opx before)))
          (incf images)
          (when msg
            (push (format nil "file, ~a: ~a" where msg) failures)))))
    (values (reverse failures) (+ total 1) images)))

(defun crash-main ()
  (let* ((base-file (concatenate 'string *oracle-workdir* "/base.img"))
         (img (concatenate 'string *oracle-workdir* "/crash.img"))
         (base (progn
                 (host-must *mkfs* "-F" "32" "-s" "1" "-n" "CRASH" "-C" base-file "40960")
                 (let* ((bytes (read-image base-file))
                        (v (fat32-open bytes)))
                   (crash-setup v)
                   (write-bytes-file base-file bytes)
                   bytes)))
         (ok t))
    (dolist (sc (crash-scenarios))
      (multiple-value-bind (failures points images)
          (run-crash-scenario sc base base-file img)
        (format t "~a: ~a crash points, ~a images, ~a failure(s)~%"
                (getf sc :name) points images (length failures))
        (when failures
          (setq ok nil)
          (dolist (f (subseq failures 0 (min 5 (length failures))))
            (format t "    ~a~%" f)))))
    (format t "FAT32-CRASH ~a~%" (if ok "PASS" "FAIL"))))

(crash-main)
