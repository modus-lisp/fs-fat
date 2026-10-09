#!/bin/sh
# run-unicode-names.sh -- names above U+FFFF: the UTF-16 helpers, FAT names written and
# read back, and exported by mtools to match, with fsck.fat clean.  User mode throughout.
#
#   test/run-unicode-names.sh

set -u

ROOT=$(cd "$(dirname "$0")/.." && pwd)
SBCL=${SBCL:-sbcl}
MKFS=${MKFS:-/sbin/mkfs.vfat}
FSCK=${FSCK:-/sbin/fsck.fat}
export MTOOLS_SKIP_CHECK=1

WORK=$(mktemp -d) || exit 1
trap 'rm -rf "$WORK"' EXIT

IMG=$WORK/u.img
"$MKFS" -F 32 -s 1 -C "$IMG" 40960 >/dev/null 2>&1 || { echo "mkfs failed"; exit 1; }

# exFAT checks run when exfatprogs is available: MKFS_EXFAT and FSCK_EXFAT, or on PATH
MKFS_EX=${MKFS_EXFAT:-mkfs.exfat}
FSCK_EX=${FSCK_EXFAT:-fsck.exfat}
EXIMG=$WORK/ux.img
EXLINE='(defvar *uni-exfat-image* nil)'
if command -v "$MKFS_EX" >/dev/null 2>&1 && command -v "$FSCK_EX" >/dev/null 2>&1; then
  truncate -s 16M "$EXIMG" && "$MKFS_EX" -n UNIUP "$EXIMG" >/dev/null 2>&1 || { echo "mkfs.exfat failed"; exit 1; }
  EXLINE="(defvar *uni-exfat-image* \"$EXIMG\")"
else
  echo "exfatprogs not found: exFAT checks left out"
fi

cat > "$WORK/run.lisp" <<EOF2
(defvar *uni-lib-fat* "$ROOT/lib/fat32.lisp")
(defvar *uni-lib-exfat* "$ROOT/lib/exfat.lisp")
(defvar *uni-image* "$IMG")
(defvar *uni-names* "$WORK/expected.txt")
(defvar *uni-exfat-image* $EXLINE)
(load "$ROOT/test/unicode-names-test.lisp")
EOF2

out=$("$SBCL" --script "$WORK/run.lisp" 2>&1)
rc=$?
echo "$out" | grep -v "^;"
[ "$rc" -eq 0 ] || { echo "UNICODE-NAMES run failed (sbcl exit $rc)"; exit 1; }
echo "$out" | grep -q '^UNICODE-NAMES PASS' || exit 1

"$FSCK" -n "$IMG" >/dev/null 2>&1 || { echo "fsck.fat -n REJECTED the image"; exit 1; }
if [ -f "$EXIMG" ]; then
  "$FSCK_EX" -n "$EXIMG" >"$WORK/fsck-ex.txt" 2>&1
  if grep -q ERROR "$WORK/fsck-ex.txt"; then
    echo "fsck.exfat -n REJECTED the exFAT image:"; head -10 "$WORK/fsck-ex.txt"; exit 1
  fi
  echo "fsck.exfat: $(tail -1 "$WORK/fsck-ex.txt")"
fi

EXPORT=$WORK/export
mkdir -p "$EXPORT"
mcopy -s -i "$IMG" :: "$EXPORT" || { echo "mtools export failed"; exit 1; }
(cd "$EXPORT" && find . -mindepth 1 -maxdepth 1 -printf '%f\n' | LC_ALL=C sort) > "$WORK/mtools.txt"
LC_ALL=C sort "$WORK/expected.txt" > "$WORK/expected-sorted.txt"
if diff "$WORK/expected-sorted.txt" "$WORK/mtools.txt" >/dev/null; then
  echo "mtools exports the same names: $(wc -l < "$WORK/mtools.txt") files"
  echo "UNICODE-NAMES PASS (fsck.fat clean)"
  exit 0
fi
echo "mtools names differ from the expected names:"
diff "$WORK/expected-sorted.txt" "$WORK/mtools.txt" | head -20
exit 1
