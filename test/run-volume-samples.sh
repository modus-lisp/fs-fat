#!/bin/sh
# run-volume-samples.sh -- the readers against two volumes written by other tools: the
# exFAT and FAT partitions of the Debian forensics-samples images, with their original
# files.  Fetches the three packages from deb.debian.org, so it needs the network;
# CACHE=DIR keeps them between runs.  User mode throughout.
#
#   test/run-volume-samples.sh
#
# Needs: curl, dpkg-deb, xz, dd, sbcl, mtools, fsck.fat, and fsck.exfat (exfatprogs;
# FSCK_EXFAT, or on PATH).

set -u

ROOT=$(cd "$(dirname "$0")/.." && pwd)
SBCL=${SBCL:-sbcl}
FSCK=${FSCK:-/sbin/fsck.fat}
FSCK_EX=${FSCK_EXFAT:-fsck.exfat}
MTOOLS_BASE=https://deb.debian.org/debian/pool/main/f/forensics-samples
export MTOOLS_SKIP_CHECK=1

command -v "$FSCK_EX" >/dev/null 2>&1 || { echo "fsck.exfat not found (set FSCK_EXFAT)"; exit 1; }
command -v "$FSCK" >/dev/null 2>&1 || { echo "fsck.fat not found (set FSCK)"; exit 1; }

WORK=$(mktemp -d) || exit 1
trap 'rm -rf "$WORK"' EXIT
CACHE=${CACHE:-$WORK/cache}
mkdir -p "$CACHE"

fetch() {   # $1 = package file name
  [ -s "$CACHE/$1" ] || curl -sfL "$MTOOLS_BASE/$1" -o "$CACHE/$1" || {
    echo "could not fetch $1"; exit 1; }
}
fetch forensics-samples-exfat_1.1.4-6_all.deb
fetch forensics-samples-vfat_1.1.4-6_all.deb
fetch forensics-samples-files_1.1.4-6_all.deb

X=$WORK/x
dpkg-deb -x "$CACHE/forensics-samples-exfat_1.1.4-6_all.deb" "$X" || exit 1
dpkg-deb -x "$CACHE/forensics-samples-vfat_1.1.4-6_all.deb" "$X" || exit 1
dpkg-deb -x "$CACHE/forensics-samples-files_1.1.4-6_all.deb" "$X" || exit 1
SHARE=$X/usr/share/forensics-samples
ORIG=$SHARE/original-files

# Each image is a whole disk with an MBR; the one partition starts at sector 2048 and
# is 100352 sectors long.
xz -dc "$SHARE/fs.exfat.xz" > "$WORK/disk.exfat" && xz -dc "$SHARE/fs.vfat.xz" > "$WORK/disk.vfat" || exit 1
dd if="$WORK/disk.exfat" of="$WORK/part.exfat" bs=512 skip=2048 count=100352 status=none
dd if="$WORK/disk.vfat" of="$WORK/part.vfat" bs=512 skip=2048 count=100352 status=none

fail=0
# fsck.exfat -n can exit 0 with ERROR lines, so the output is checked as well
"$FSCK_EX" -n "$WORK/part.exfat" > "$WORK/fsck-exfat.txt" 2>&1
if [ $? -ne 0 ] || grep -q ERROR "$WORK/fsck-exfat.txt"; then
  echo "fsck.exfat rejects the exFAT partition"; fail=1
fi
"$FSCK" -n "$WORK/part.vfat" >/dev/null 2>&1 || { echo "fsck.fat rejects the FAT partition"; fail=1; }

EXPORT=$WORK/ours
mkdir -p "$EXPORT"
cat > "$WORK/run.lisp" <<EOF2
(defvar *vs-fat-lib* "$ROOT/lib/fat32.lisp")
(defvar *vs-exfat-lib* "$ROOT/lib/exfat.lisp")
(defvar *vs-exfat-part* "$WORK/part.exfat")
(defvar *vs-fat-part* "$WORK/part.vfat")
(defvar *vs-original* "$ORIG")
(defvar *vs-export* "$EXPORT")
(load "$ROOT/test/volume-samples-test.lisp")
EOF2
out=$("$SBCL" --script "$WORK/run.lisp" 2>&1)
rc=$?
echo "$out" | grep -v "^;"
[ "$rc" -eq 0 ] && echo "$out" | grep -q '^VOLUME-SAMPLES PASS' || fail=1

# the FAT reader's tree, against mtools' export of the same volume
MT=$WORK/mtools
mkdir -p "$MT"
mcopy -s -i "$WORK/part.vfat" :: "$MT" || { echo "mtools export failed"; fail=1; }
if diff -r "$EXPORT" "$MT" >/dev/null 2>&1; then
  echo "FAT reader matches mtools on every file"
else
  echo "FAT reader and mtools differ:"; diff -r "$EXPORT" "$MT" | head -10; fail=1
fi

[ "$fail" -eq 0 ] && echo "VOLUME-SAMPLES RUN PASS" && exit 0
echo "VOLUME-SAMPLES RUN FAIL"; exit 1
