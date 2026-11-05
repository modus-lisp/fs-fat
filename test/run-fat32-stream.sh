#!/bin/sh
# run-fat32-stream.sh -- FAT32-WRITE-FILE-FROM: a file written in chunks, checked by mtools and
# fsck.fat.  A 21 MB file in 1 MB chunks must export byte for byte; a replacement must replace;
# too little or too much data must fail and leave no lost clusters.  Exit 0 only if all pass.
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
(defun chunks (path size)
  (let ((s (open path :element-type '(unsigned-byte 8))))
    (lambda () (let* ((b (make-array size :element-type '(unsigned-byte 8))) (n (read-sequence b s)))
                 (if (zerop n) (progn (close s) nil) (subseq b 0 n))))))
(defun pieces (total piece)
  (let ((k 0)) (lambda () (when (< k total) (let ((n (min piece (- total k)))) (incf k n)
                                             (make-array n :element-type '(unsigned-byte 8) :initial-element 65))))))
(multiple-value-bind (d s) (dev "$W/img")
  (let ((v (fat32-open-device d)) (bad 0))
    (fat32-mkdir v "/boot")
    (fat32-write-file-from v "/boot/big.bin" 21000123 (chunks "$W/data.bin" 1048576))
    (fat32-write-file v "/boot/small.txt" (map '(vector (unsigned-byte 8)) #'char-code "old contents"))
    (fat32-write-file-from v "/boot/small.txt" 5000 (pieces 5000 1700))
    (unless (handler-case (progn (fat32-write-file-from v "/boot/short" 10000 (pieces 100 100)) nil) (error () t)) (incf bad))
    (unless (handler-case (progn (fat32-write-file-from v "/boot/long" 10 (pieces 200 100)) nil) (error () t)) (incf bad))
    (when (fat32-lookup v "/boot/short") (incf bad))
    (when (fat32-lookup v "/boot/long") (incf bad))
    (fat32-sync v)
    (close s)
    (sb-ext:exit :code bad)))
LISP
sbcl --noinform --non-interactive --load "$W/t.lisp" >/dev/null 2>&1 || { echo "FAIL: the library run (or a short/long write that did not fail)"; exit 1; }
mcopy -o -i "$W/img" ::/boot/big.bin "$W/out.bin" && cmp -s "$W/data.bin" "$W/out.bin" || { echo "FAIL: big.bin differs"; exit 1; }
mcopy -o -i "$W/img" ::/boot/small.txt "$W/small" && [ "$(wc -c < "$W/small")" = 5000 ] && [ "$(tr -d A < "$W/small" | wc -c)" = 0 ] || { echo "FAIL: small.txt"; exit 1; }
/sbin/fsck.fat -n "$W/img" >/dev/null || { echo "FAIL: fsck.fat"; exit 1; }
echo "PASS run-fat32-stream"
