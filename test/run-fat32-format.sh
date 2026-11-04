#!/bin/sh
# run-fat32-format.sh -- FAT32-FORMAT a 2 GiB sparse image (test/fat32-format-test.lisp),
# then have mtools and fsck.fat read it cold.  Exit 0 = pass.
set -eu
HERE=$(cd "$(dirname "$0")" && pwd)
IMG=$(mktemp -d)/fmt.img
trap 'rm -rf "$(dirname "$IMG")"' EXIT
sbcl --script "$HERE/fat32-format-test.lisp" "$IMG" 2147483648
MTOOLS_SKIP_CHECK=1 mtype -i "$IMG" ::/pkgs/hello.lisp | grep -q ':hello' || { echo "mtools: FAIL"; exit 1; }
echo "mtools: reads the volume"
FSCK=$(command -v fsck.fat || ls /usr/sbin/fsck.fat /sbin/fsck.fat 2>/dev/null | head -1 || true)
if [ -n "$FSCK" ]; then "$FSCK" -n "$IMG" && echo "fsck.fat: clean"; else echo "fsck.fat: not installed, skipped"; fi
