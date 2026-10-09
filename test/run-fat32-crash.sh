#!/bin/sh
# run-fat32-crash.sh -- crash consistency of lib/fat32.lisp: every crash point of
# every operation, with the unflushed writes persisted in several ways, each image
# repaired by fsck.fat -a and checked against the operation's contract.
#
#   test/run-fat32-crash.sh [RANDOM_TRIALS]     (default 2 random subsets per point)
#
# The crash is modelled, not a power cut: see the header of test/fat32-crash.lisp.
# User mode throughout.  Exit status 0 only if the run prints FAT32-CRASH PASS.

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
(load "$ROOT/test/fat32-crash.lisp")
EOF2

out=$("$SBCL" --script "$WORK/run.lisp" 2>&1)
rc=$?
echo "$out" | grep -v "^;"
if [ "$rc" -eq 0 ] && echo "$out" | grep -q '^FAT32-CRASH PASS'; then
  exit 0
fi
echo "FAT32-CRASH did not pass (sbcl exit $rc)" >&2
exit 1
