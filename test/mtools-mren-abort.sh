#!/bin/sh
# mtools-mren-abort.sh -- minimal reproducer for an mtools 4.0.32 abort on rename.
#
#   test/mtools-mren-abort.sh [MKFS]
#
# A fresh FAT32 root directory holds f1 and f2.  f2 is deleted, then f1 is renamed
# to a name longer than 13 characters.  That name needs three directory slots
# (two long-name entries and the short entry), and only two are free, which is
# where mren aborts:
#   mren: dirCache.c:198: freeDirCacheRange: Assertion `entry->beginSlot == beginSlot' failed.
# The abort is deterministic, and the image is left byte-identical to its state
# before the rename, so the volume itself is not damaged.
#
# This is an oracle-tool bug, not a bug in lib/fat32.lisp.  test/fat32-ops.lisp
# classifies exactly this abort (SIGABRT from mren) as a known tool bug, and
# checks that the volume is unchanged before it skips the operation.
#
# Exit 0 when the abort reproduces, 1 when it does not (the tool may have been fixed).

set -u

MKFS=${1:-${MKFS:-/sbin/mkfs.vfat}}
export MTOOLS_SKIP_CHECK=1

WORK=$(mktemp -d) || exit 1
trap 'rm -rf "$WORK"' EXIT
cd "$WORK" || exit 1

echo test > s
"$MKFS" -F 32 -s 1 -n T -C pre.img 40960 >/dev/null || { echo "mkfs failed"; exit 1; }
mcopy -i pre.img s ::f1
mcopy -i pre.img s ::f2
mdel -i pre.img ::f2

cp pre.img v.img
mren -i v.img ::f1 ::qqqqqqqqqqqqqqq >/dev/null 2>&1
rc=$?

if [ "$rc" -eq 134 ] && cmp -s pre.img v.img; then
  echo "mtools-mren-abort: REPRODUCED (SIGABRT, image unchanged)"
  exit 0
fi
if [ "$rc" -eq 134 ]; then
  echo "mtools-mren-abort: aborted, but the image CHANGED"
  exit 1
fi
echo "mtools-mren-abort: not reproduced (mren exit $rc)"
exit 1
