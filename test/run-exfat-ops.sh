#!/bin/sh
# run-exfat-ops.sh -- random operations on an exFAT volume, checked against a model by
# the reader after every few operations, and by fsck.exfat -n.  User mode throughout.
#
#   test/run-exfat-ops.sh [SEEDS] [FIRST_SEED] [COUNT] [EVERY]
#
# Needs mkfs.exfat and fsck.exfat (exfatprogs); set MKFS_EXFAT / FSCK_EXFAT to their paths.

set -u

ROOT=$(cd "$(dirname "$0")/.." && pwd)
SBCL=${SBCL:-sbcl}
SEEDS=${1:-3}
FIRST=${2:-1}
COUNT=${3:-200}
EVERY=${4:-10}

WORK=$(mktemp -d) || exit 1
trap 'rm -rf "$WORK"' EXIT

cat > "$WORK/run.lisp" <<EOF2
(defvar *oracle-root* "$ROOT/")
(defvar *oracle-workdir* "$WORK")
(defvar *ops-first-seed* $FIRST)
(defvar *ops-seeds* $SEEDS)
(defvar *ops-count* $COUNT)
(defvar *ops-every* $EVERY)
(defvar *mkfs-exfat* "${MKFS_EXFAT:-mkfs.exfat}")
(defvar *fsck-exfat* "${FSCK_EXFAT:-fsck.exfat}")
(load "$ROOT/test/exfat-ops.lisp")
EOF2

out=$("$SBCL" --script "$WORK/run.lisp" 2>&1)
rc=$?
echo "$out" | grep -v "^;"
if [ "$rc" -eq 0 ] && echo "$out" | grep -q '^EXFAT-OPS PASS'; then
  exit 0
fi
echo "EXFAT-OPS did not pass (sbcl exit $rc)" >&2
exit 1
