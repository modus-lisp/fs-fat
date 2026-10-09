#!/bin/sh
# run-fat32-write.sh -- lib/fat32.lisp writes, checked by fsck.fat and mtools.
#
#   test/run-fat32-write.sh [SIZE_KIB]     (default 40960: one cluster per sector)
#
# test/fat32-write-test.lisp writes a workload through the library, and into a host
# mirror in step.  Then fsck.fat must accept the image, and an mtools export of the
# image must equal the mirror byte for byte.  User mode throughout.
#
# Exit status 0 only if every check passes.

set -u

ROOT=$(cd "$(dirname "$0")/.." && pwd)
SBCL=${SBCL:-sbcl}
MKFS=${MKFS:-/sbin/mkfs.vfat}
FSCK=${FSCK:-/sbin/fsck.fat}
SIZE=${1:-40960}
export MTOOLS_SKIP_CHECK=1

WORK=$(mktemp -d) || exit 1
trap 'rm -rf "$WORK"' EXIT

IMG=$WORK/v.img
MIRROR=$WORK/mirror
EXPORT=$WORK/export
mkdir -p "$MIRROR" "$EXPORT"

"$MKFS" -F 32 -s 1 -n WRITETEST -C "$IMG" "$SIZE" >/dev/null 2>&1 || {
  echo "mkfs.vfat failed"; exit 1; }

cat > "$WORK/run.lisp" <<EOF2
(defvar *write-lib* "$ROOT/lib/fat32.lisp")
(defvar *write-image* "$IMG")
(defvar *write-mirror* "$MIRROR")
(load "$ROOT/test/fat32-write-test.lisp")
EOF2

out=$("$SBCL" --script "$WORK/run.lisp" 2>&1)
rc=$?
echo "$out" | grep -v "^;" | tail -15
[ "$rc" -eq 0 ] || { echo "write workload failed (sbcl exit $rc)"; exit 1; }

fail=0
"$FSCK" -n "$IMG" || fail=1
echo "fsck: $([ $fail -eq 0 ] && echo clean || echo REJECTED)"

mcopy -s -i "$IMG" :: "$EXPORT" || { echo "mtools export failed"; exit 1; }
if diff -r "$MIRROR" "$EXPORT" >"$WORK/diff.txt" 2>&1; then
  echo "export matches the mirror ($(find "$MIRROR" -type f | wc -l) files)"
else
  echo "export DIFFERS from the mirror:"; head -20 "$WORK/diff.txt"; fail=1
fi

if [ "$fail" -eq 0 ]; then echo "FAT32-WRITE PASS"; exit 0; fi
echo "FAT32-WRITE FAIL"; exit 1
