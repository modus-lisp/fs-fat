#!/bin/sh
# run-fat32.sh -- lib/fat32.lisp against volumes made by mkfs.vfat and filled
# by mtools (no mount, no root).
#
#   test/run-fat32.sh
#
# Two volumes: 512-byte clusters (1 sector each) and 4 KiB clusters (8 sectors
# each).  The source tree has a long mixed-case name, an empty file, a file of
# exactly one 4 KiB cluster, a 3 MiB file spanning many clusters, a nested
# directory, and a 300-entry directory whose listing spans several clusters.
# gone.txt is copied in and then deleted from the image, so it must not appear.
#
# The test itself (test/fat32-test.lisp) is portable CL.  This runner is
# host-side and uses SBCL to run it; the inputs reach the test as defvars in a
# generated wrapper file.
#
# Exit status 0 only if both volumes report FAT32-TEST PASS.

set -u

ROOT=$(cd "$(dirname "$0")/.." && pwd)
SBCL=${SBCL:-sbcl}
MKFS=${MKFS:-/sbin/mkfs.vfat}
export MTOOLS_SKIP_CHECK=1

WORK=$(mktemp -d) || exit 1
trap 'rm -rf "$WORK"' EXIT

SRC=$WORK/src
mkdir -p "$SRC/sub/deeper" "$SRC/many"
printf 'hello\n' > "$SRC/small.txt"
printf 'long name\n' > "$SRC/Mixed Case Name.Text"
: > "$SRC/empty.bin"
head -c 4096 /dev/urandom > "$SRC/exact.bin"
head -c 3145728 /dev/urandom > "$SRC/big.bin"
printf 'deep\n' > "$SRC/sub/deeper/x.dat"
i=1
while [ "$i" -le 300 ]; do
  printf '%d\n' "$i" > "$SRC/many/file-$(printf %04d "$i").dat"
  i=$((i + 1))
done
printf 'gone\n' > "$SRC/gone.txt"

# Ground truth: every source entry except gone.txt, as "TYPE RELPATH".
(cd "$SRC" && find . -mindepth 1 ! -name gone.txt -printf '%y %P\n' | sort) > "$WORK/manifest.txt"

overall=0

run_case() {   # $1 label   $2 volume size in KiB   $3 sectors per cluster
  img=$WORK/$1.img
  "$MKFS" -F 32 -s "$3" -n FATTEST -C "$img" "$2" >/dev/null 2>&1 || {
    echo "$1: mkfs.vfat failed"; overall=1; return; }
  # shellcheck disable=SC2086
  mcopy -s -i "$img" "$SRC"/* :: || { echo "$1: mcopy failed"; overall=1; return; }
  mdel -i "$img" ::gone.txt || { echo "$1: mdel failed"; overall=1; return; }

  cat > "$WORK/run-$1.lisp" <<EOF
(defvar *fat-lib* "$ROOT/lib/fat32.lisp")
(defvar *fat-image* "$img")
(defvar *fat-manifest* "$WORK/manifest.txt")
(defvar *fat-src* "$SRC")
(load "$ROOT/test/fat32-test.lisp")
EOF
  out=$("$SBCL" --script "$WORK/run-$1.lisp" 2>&1)
  rc=$?
  echo "== $1 ($3 sectors/cluster, $2 KiB) =="
  echo "$out"
  if [ "$rc" -eq 0 ] && echo "$out" | grep -q '^FAT32-TEST PASS'; then
    echo "$1: PASS"
  else
    echo "$1: FAIL (sbcl exit $rc)"
    overall=1
  fi
}

run_case small 40960 1
run_case cluster4k 307200 8

exit "$overall"
