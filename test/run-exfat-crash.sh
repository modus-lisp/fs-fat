#!/bin/sh
# run-exfat-crash.sh -- crash consistency of lib/exfat.lisp: every crash point of
# every sequence of operations, with the unflushed writes persisted in several ways,
# each image repaired by fsck.exfat -a and checked against the operation's contract.
#
#   test/run-exfat-crash.sh [RANDOM_TRIALS]     (default 2)
#
# Needs mkfs.exfat and fsck.exfat (MKFS_EXFAT, FSCK_EXFAT, or on PATH).  The crash is
# modelled: see the header of test/exfat-crash.lisp.  User mode throughout.

set -u

ROOT=$(cd "$(dirname "$0")/.." && pwd)
SBCL=${SBCL:-sbcl}
TRIALS=${1:-2}

WORK=$(mktemp -d) || exit 1
trap 'rm -rf "$WORK"' EXIT

cat > "$WORK/run.lisp" <<EOF2
(defvar *oracle-root* "$ROOT/")
(defvar *oracle-workdir* "$WORK")
(defvar *crash-random-trials* $TRIALS)
(defvar *mkfs-exfat* "${MKFS_EXFAT:-mkfs.exfat}")
(defvar *fsck-exfat* "${FSCK_EXFAT:-fsck.exfat}")
(load "$ROOT/test/exfat-crash.lisp")
EOF2

out=$("$SBCL" --script "$WORK/run.lisp" 2>&1)
rc=$?
echo "$out" | grep -v "^;"
if [ "$rc" -eq 0 ] && echo "$out" | grep -q '^EXFAT-CRASH PASS'; then
  exit 0
fi
echo "EXFAT-CRASH did not pass (sbcl exit $rc)" >&2
exit 1
