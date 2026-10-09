#!/bin/sh
# run-exfat-write.sh -- lib/exfat.lisp writes, checked by fsck.exfat -n and by re-reading
# the image against the expected tree.  Uses exfatprogs (mkfs.exfat, fsck.exfat) from
# EXFATPROGS, or from PATH.  User mode throughout.
#
#   test/run-exfat-write.sh [SIZE]        (default 64M)

set -u

ROOT=$(cd "$(dirname "$0")/.." && pwd)
SBCL=${SBCL:-sbcl}
MKFS=${MKFS_EXFAT:-mkfs.exfat}
FSCK=${FSCK_EXFAT:-fsck.exfat}
SIZE=${1:-64M}

WORK=$(mktemp -d) || exit 1
trap 'rm -rf "$WORK"' EXIT

IMG=$WORK/v.img
truncate -s "$SIZE" "$IMG" || exit 1
"$MKFS" -n EXWRITE "$IMG" >/dev/null 2>&1 || { echo "mkfs.exfat failed"; exit 1; }

cat > "$WORK/run.lisp" <<EOF2
(defvar *write-lib* "$ROOT/lib/fat32.lisp")
(defvar *exfat-lib* "$ROOT/lib/exfat.lisp")
(defvar *write-image* "$IMG")
(defvar *write-mirror* "$WORK/mirror")
(load "$ROOT/test/exfat-write-test.lisp")
EOF2

out=$("$SBCL" --script "$WORK/run.lisp" 2>&1)
rc=$?
echo "$out" | grep -v "^;" | tail -15
[ "$rc" -eq 0 ] || { echo "write workload failed (sbcl exit $rc)"; exit 1; }

"$FSCK" -n "$IMG" || { echo "fsck.exfat -n REJECTED the image"; exit 1; }
echo "$out" | grep -q "^EXFAT-WRITE-TEST PASS" || { echo "EXFAT-WRITE-TEST FAIL"; exit 1; }
echo "EXFAT-WRITE PASS"
