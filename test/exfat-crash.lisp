;;;; test/exfat-crash.lisp -- crash consistency of lib/exfat.lisp.
;;;;
;;;; The same method as test/fat32-crash.lisp: each scenario is a sequence of operations
;;;; run on one volume, each synced at its end, and the disk dies at every event in
;;;; turn (memory simulation, with the unflushed writes persisted in several subsets;
;;;; and a real image file).  Each crash image is repaired with fsck.exfat -a and must
;;;; then be clean under fsck.exfat -n: its exit status is 0 even when it reports
;;;; errors, so the harness also checks that no ERROR line is printed.
;;;;
;;;; The contract, per operation the crash interrupts:
;;;;
;;;;   every operation before it is durable;
;;;;   a single path is in its state before the operation or its state after it;
;;;;   a rename is visible under the old name or the new name, or under neither: then
;;;;     its clusters stay allocated and unreferenced.  fsck.exfat -a does not reclaim
;;;;     them, so the data is leaked, not recovered;
;;;;   a directory moved to another parent keeps its contents under each name it is
;;;;     visible under;
;;;;   every path the operation does not touch is unchanged.
;;;;
;;;; Runner: test/run-exfat-crash.sh.  It defines before loading this file:
;;;;   *oracle-root*  *oracle-workdir*  *crash-random-trials*
;;;; and may set *mkfs-exfat* and *fsck-exfat*.

(defvar *oracle-library-only* t)
(defvar *mkfs-exfat* "mkfs.exfat")
(defvar *fsck-exfat* "fsck.exfat")
(load (concatenate 'string *oracle-root* "test/fat32-oracle.lisp"))
(use-package :fs-fat)
(load (concatenate 'string *oracle-root* "test/fat32-hostdev.lisp"))
(load (concatenate 'string *oracle-root* "lib/exfat.lisp"))

(defun exfat-open-device (dev)
  (exfat-open (svref dev 0) dev))

(defun crash-pat (n seed)
  (let ((v (make-array n :element-type '(unsigned-byte 8))))
    (dotimes (i n) (setf (aref v i) (mod (+ (* i 31) seed) 251)))
    v))

(defun crash-base-paths ()
  '("old.txt" "keep.txt" "d" "e" "d/sub" "d/keepme.bin" "d/sub/inner.txt" "e/empty"))

(defun crash-setup (v)
  (exfat-mkdir v "d")
  (exfat-mkdir v "e")
  (exfat-mkdir v "d/sub")
  (exfat-mkdir v "e/empty")
  (exfat-write-file v "old.txt" (crash-pat 1500 1))
  (exfat-write-file v "keep.txt" (crash-pat 900 2))
  (exfat-write-file v "d/keepme.bin" (crash-pat 2000 3))
  (exfat-write-file v "d/sub/inner.txt" (crash-pat 700 4))
  (exfat-sync v))

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
  (crash-op name :single path :op (lambda (v) (exfat-write-file v path (crash-pat n seed)))))

(defun crash-delete (name path)
  (crash-op name :single path :op (lambda (v) (exfat-delete v path))))

(defun crash-rename (name old new &key child also)
  (crash-op name :rename (list old new) :child child :also also
            :op (lambda (v) (exfat-rename v old new))))

(defun crash-mkdir (name path)
  (crash-op name :single path :op (lambda (v) (exfat-mkdir v path))))

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
  (let ((v (exfat-open-device dev))
        (bounds nil))
    (dolist (op ops)
      (push (device-events dev) bounds)
      (handler-case (progn (funcall (getf op :op) v) (exfat-sync v))
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
  (let ((v (exfat-open view)))
    (mapcar (lambda (p) (cons p (path-state v p))) paths)))

(defun crash-keeps (k npending)
  "Which unflushed writes survive a crash at point K, of NPENDING: none; all; each one
   alone (which is how a later write can outlive an earlier one, deterministically);
   and random subsets, each a function of the write's position."
  (append (list :none :all)
          (loop for j from 0 below npending
                collect (let ((only j))
                          (lambda (i) (= i only))))
          (loop for r from 1 to *crash-random-trials*
                collect (let ((rng (rng-make (+ 1000 (* 97 k) (* 13 r)))))
                          (lambda (i) (declare (ignore i)) (rng-chance rng 0.5))))))

(defun keep-name (keep)
  (cond ((eq keep :none) "none persisted")
        ((eq keep :all) "all persisted")
        (t "some unflushed writes persisted")))

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
  (let ((e (exfat-lookup v p)))
    (cond ((null e) nil)
          ((nth 1 e) (list :dir))
          (t (let ((d (exfat-read-file v p)))
               (list :file (wk-fnv d) (length d)))))))

(defun crash-contract (v op base-states)
  "Check the volume V against OP's contract.  BASE-STATES gives the state of each path
   before OP.  Returns a failure message, or NIL.  A post-crash file, if present, must
   read back as written."
  (let ((post (exfat-lookup v "post-crash.bin")))
    (when (and post
               (not (equalp (exfat-read-file v "post-crash.bin") (crash-pat 200000 99))))
      (return-from crash-contract "the post-crash file reads back wrong")))
  (handler-case
      (progn
        (dolist (p (mapcar #'car base-states))
          (unless (member p (crash-touched op) :test #'string=)
            (let ((now (path-state v p))
                  (was (cdr (assoc p base-states :test #'string=))))
              (unless (equal now was)
                (return-from crash-contract
                  (format nil "untouched ~s changed: ~s -> ~s" p was now))))))
        (cond
          ((getf op :single)
           (let* ((p (getf op :single))
                  (before (cdr (assoc p base-states :test #'string=)))
                  (now (path-state v p)))
             (unless (or (equal now before) (equal now (getf op :after)))
               (return-from crash-contract
                 (format nil "~s is ~s, expected ~s or ~s"
                         p now before (getf op :after))))))
          (t
           (destructuring-bind (old new) (getf op :rename)
             (let* ((before (cdr (assoc old base-states :test #'string=)))
                    (a (path-state v old))
                    (b (path-state v new)))
               (unless (or (and (equal a before) (null b))
                           (and (null a) (equal b before))
                           (and (equal a before) (equal b before))
                           ;; the data is leaked, not lost: its clusters stay allocated
                           (and (null a) (null b)))
                 (return-from crash-contract
                   (format nil "rename ~s -> ~s left ~s and ~s" old new a b)))
               (when (getf op :child)
                 (let ((inner (cdr (assoc "d/sub/inner.txt" base-states :test #'string=))))
                   (dolist (loc (list old new))
                     (when (equal (path-state v loc) '(:dir))
                       (let ((child (path-state v (concatenate 'string loc "/inner.txt"))))
                         (unless (equal child inner)
                           (return-from crash-contract
                             (format nil "moved directory ~s lost its contents: ~s"
                                     loc child))))))))))))
        nil)
    (error (e)
      (format nil "lookup failed: ~a" e))))

(defun crash-raw-use-check (img base-states)
  "Write into the crash image as it is, before any repair, as a driver that does not
   check would.  Whatever reads correctly before the write must read the same after
   it: an allocation must never take a cluster that a live entry still refers to.
   Returns a failure message, or NIL."
  (let* ((bytes (read-image img))
         (v (handler-case (exfat-open bytes)
              (error () (return-from crash-raw-use-check nil))))
         (before (mapcar (lambda (p)
                           (cons p (handler-case (path-state v p)
                                     (error () :unreadable))))
                         (mapcar #'car base-states))))
    (handler-case (exfat-write-file v "post-raw.bin" (crash-pat 200000 77))
      (error (e)
        (return-from crash-raw-use-check
          (format nil "write before repair failed: ~a" e))))
    (let ((v2 (exfat-open bytes)))
      (dolist (b before nil)
        (unless (eq (cdr b) :unreadable)
          (let ((now (handler-case (path-state v2 (car b))
                       (error () :unreadable))))
            (unless (equal now (cdr b))
              (return-from crash-raw-use-check
                (format nil "write before repair changed ~s: ~s -> ~s"
                        (car b) (cdr b) now)))))))))

(defun crash-check-file (img op base-states)
  "Repair the crash image at IMG and check it against OP.  The repaired image must be
   clean and meet OP's contract, both as it is and after a write that reuses whatever
   the crash left free.  Returns a failure message, or NIL."
  (let ((raw (crash-raw-use-check img base-states)))
    (when raw
      (return-from crash-check-file raw)))
  (let ((fixed (host-run *fsck-exfat* "-a" img)))
    (unless (<= fixed 1)                ; 1: errors were corrected
      (return-from crash-check-file (format nil "fsck.exfat -a exited ~a" fixed))))
  (multiple-value-bind (code out) (host-run *fsck-exfat* "-n" img)
    (unless (and (= code 0) (not (search "ERROR" out)))
      (return-from crash-check-file
        (format nil "not clean after repair: ~a" (subseq out 0 (min 200 (length out)))))))
  (let* ((bytes (read-image img))
         (msg (crash-contract (exfat-open bytes) op base-states)))
    (or msg
        ;; Use after repair: the new file takes the lowest free clusters.  An entry that
        ;; still points at clusters marked free would now lose its data.
        (handler-case (progn (exfat-write-file (exfat-open bytes) "post-crash.bin"
                                               (crash-pat 200000 99))
                             nil)
          (error (e) (format nil "post-crash write failed: ~a" e)))
        (crash-contract (exfat-open bytes) op base-states))))

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
          (dolist (keep (crash-keeps k (length (svref dev 2))))
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
                 (host-must "truncate" "-s" "16M" base-file)
                 (host-must *mkfs-exfat* "-n" "CRASH" base-file)
                 (let* ((bytes (read-image base-file))
                        (v (exfat-open bytes)))
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
    (format t "EXFAT-CRASH ~a~%" (if ok "PASS" "FAIL"))))

(crash-main)
