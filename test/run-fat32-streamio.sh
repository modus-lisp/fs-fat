#!/bin/sh
# run-fat32-streamio.sh -- FAT32-WRITE-FILE-STREAM (length unknown in advance, one reused chunk
# buffer) and FAT32-READ-INTO (byte ranges), checked by mtools and fsck.fat.  A 21 MB file
# streamed in odd-sized pieces of one reused buffer must export byte for byte; random ranges
# read back must match the source; an empty stream makes an empty file; a replacement replaces;
# a NEXT that signals part way leaves no file and no lost clusters.  Exit 0 only if all pass.
set -u
ROOT=$(cd "$(dirname "$0")/.." && pwd); W=$(mktemp -d); trap 'rm -rf "$W"' EXIT
export MTOOLS_SKIP_CHECK=1
/sbin/mkfs.vfat -F 32 -C "$W/img" 300000 >/dev/null || exit 1
head -c 21000123 /dev/urandom > "$W/data.bin"
cat > "$W/t.lisp" <<LISP
(load "$ROOT/lib/fat32.lisp")
(in-package :fs-fat)
(defun dev (path)
  (let* ((s (open path :direction :io :element-type '(unsigned-byte 8) :if-exists :overwrite))
         (view (make-array (file-length s) :element-type '(unsigned-byte 8))))
    (read-sequence view s)
    (values (make-fat32-device view (lambda (off src) (file-position s off) (write-sequence src s))
                               (lambda () (finish-output s))) s)))
(defun reused-chunks (path)
  "One 65536-byte buffer, refilled with an odd number of bytes each time."
  (let ((s (open path :element-type '(unsigned-byte 8)))
        (b (make-array 65536 :element-type '(unsigned-byte 8)))
        (sizes #(7777 1 512 65536 40000 3)) (i 0))
    (lambda () (let ((n (read-sequence b s :end (svref sizes (mod (incf i) 6)))))
                 (if (zerop n) (progn (close s) nil) (values b n))))))
(defun pieces (total piece)
  (let ((k 0)) (lambda () (when (< k total) (let ((n (min piece (- total k)))) (incf k n)
                                             (make-array n :element-type '(unsigned-byte 8) :initial-element 65))))))
(defvar *bad* 0)
(defmacro check (form) \`(unless ,form (format t "FAIL: ~s~%" ',form) (incf *bad*)))
(multiple-value-bind (d s) (dev "$W/img")
  (let ((v (fat32-open-device d))
        (src (with-open-file (f "$W/data.bin" :element-type '(unsigned-byte 8))
               (let ((a (make-array (file-length f) :element-type '(unsigned-byte 8)))) (read-sequence a f) a))))
    (fat32-mkdir v "/p")
    (check (= 21000123 (fat32-write-file-stream v "/p/big.bin" (reused-chunks "$W/data.bin"))))
    (let ((h (fat32-open-input v "/p/big.bin")) (buf (make-array 200000 :element-type '(unsigned-byte 8))))
      (check (= (fat32-input-size h) 21000123))
      (dolist (r '((0 1) (0 200000) (511 2) (4095 4097) (123456 100000) (20999000 200000) (21000122 5) (21000123 9)))
        (let* ((off (first r)) (n (second r)) (got (fat32-read-into h off buf :start 7 :end (+ 7 (min n 199993)))))
          (check (= got (max 0 (min n 199993 (- 21000123 off)))))
          (check (equalp (subseq buf 7 (+ 7 got)) (subseq src off (+ off got)))))))
    (check (= 0 (fat32-write-file-stream v "/p/empty" (lambda () nil))))
    (check (equalp (fat32-read-file v "/p/empty") #()))
    (fat32-write-file v "/p/small.txt" (map '(vector (unsigned-byte 8)) #'char-code "old contents"))
    (check (= 5000 (fat32-write-file-stream v "/p/small.txt" (pieces 5000 1700))))
    (let ((k 0))
      (check (handler-case (progn (fat32-write-file-stream v "/p/broken"
                                    (lambda () (if (< (incf k) 40) (make-array 30000 :element-type '(unsigned-byte 8))
                                                   (error "network went away"))))
                                  nil)
               (error () t))))
    (check (null (fat32-lookup v "/p/broken")))
    ;; the push-style handle: two outputs open at once in one directory, another file written
    ;; in it meanwhile, then one closed and one aborted
    (let ((a (fat32-open-output v "/p/a.bin")) (b (fat32-open-output v "/p/b.bin"))
          (chunk (subseq src 0 70001)))
      (dotimes (i 30) (fat32-write-output a chunk :start (* i 13) :end (+ 60000 (* i 13)))
                      (fat32-write-output b chunk :end 333))
      (fat32-write-file v "/p/between.txt" (map '(vector (unsigned-byte 8)) #'char-code "written while two were open"))
      (check (null (fat32-lookup v "/p/a.bin")))
      (check (= (fat32-close-output a) (* 30 (- 60000 0))))
      (fat32-close-output b :abort t)
      (check (null (fat32-lookup v "/p/b.bin")))
      (let ((got (fat32-read-file v "/p/a.bin")))
        (check (= (length got) 1800000))
        (check (loop for i below 30 always (equalp (subseq got (* i 60000) (* (1+ i) 60000))
                                                   (subseq chunk (* i 13) (+ 60000 (* i 13))))))))
    (fat32-sync v)
    (close s)
    (sb-ext:exit :code *bad*)))
LISP
out=$(sbcl --noinform --non-interactive --load "$W/t.lisp" 2>&1); rc=$?; printf "%s\n" "$out" | grep -i "fail\|error" | head -5
[ "$rc" = 0 ] || { echo "FAIL: the library run ($rc)"; exit 1; }
mcopy -o -i "$W/img" ::/p/big.bin "$W/out.bin" && cmp -s "$W/data.bin" "$W/out.bin" || { echo "FAIL: big.bin differs"; exit 1; }
mcopy -o -i "$W/img" ::/p/small.txt "$W/small" && [ "$(wc -c < "$W/small")" = 5000 ] && [ "$(tr -d A < "$W/small" | wc -c)" = 0 ] || { echo "FAIL: small.txt"; exit 1; }
/sbin/fsck.fat -n "$W/img" >/dev/null || { echo "FAIL: fsck.fat"; /sbin/fsck.fat -n "$W/img" | tail -5; exit 1; }
mcopy -o -i "$W/img" ::/p/between.txt "$W/between" && [ "$(cat "$W/between")" = "written while two were open" ] || { echo "FAIL: between.txt"; exit 1; }
[ "$(mdir -b -i "$W/img" ::/p/ 2>/dev/null | grep -c 'a.bin')" = 1 ] || { echo "FAIL: a.bin entry"; exit 1; }
echo "PASS run-fat32-streamio"
