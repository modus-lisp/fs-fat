#!/bin/sh
# run-fat32-oracle.sh -- run test/fat32-oracle.lisp: mkfs.vfat, mtools and
# fsck.fat as the oracle for lib/fat32.lisp, plus a patch fuzzer.
#
#   test/run-fat32-oracle.sh [SEEDS] [FIRST_SEED] [CASES]
#
#   SEEDS       consecutive seeds to run            (default 3)
#   FIRST_SEED  the first seed                      (default 1)
#   CASES       fuzz cases per seed                 (default 200)
#   KEEP=DIR    keep each seed's image and failures in DIR (default: discard)
#
# A seed fixes everything: tree, cluster size, deletions, and every fuzz case.
# Re-running with the same arguments reproduces the same run.
#
# Host-side only: this script and the SBCL invocation.  The oracle itself is
# test/fat32-oracle.lisp, which is portable CL apart from its one host-run seam.
#
# Exit status 0 only if the run prints FAT32-ORACLE PASS.

set -u

ROOT=$(cd "$(dirname "$0")/.." && pwd)
SBCL=${SBCL:-sbcl}
SEEDS=${1:-3}
FIRST=${2:-1}
CASES=${3:-200}

WORK=$(mktemp -d) || exit 1
if [ -n "${KEEP:-}" ]; then
  mkdir -p "$KEEP"
  KEEPDIR=$(cd "$KEEP" && pwd)
  WORKDIR=$KEEPDIR/run-$$
  mkdir -p "$WORKDIR"
else
  WORKDIR=$WORK/work
  mkdir -p "$WORKDIR"
fi
trap 'rm -rf "$WORK"' EXIT

cat > "$WORK/run.lisp" <<EOF
(defvar *oracle-root* "$ROOT/")
(defvar *oracle-workdir* "$WORKDIR")
(defvar *oracle-first-seed* $FIRST)
(defvar *oracle-seeds* $SEEDS)
(defvar *oracle-cases* $CASES)
(load "$ROOT/test/fat32-oracle.lisp")
EOF

out=$("$SBCL" --script "$WORK/run.lisp" 2>&1)
rc=$?
echo "$out"
if [ -n "${KEEP:-}" ]; then
  echo "kept in: $WORKDIR"
fi

if [ "$rc" -eq 0 ] && echo "$out" | grep -q '^FAT32-ORACLE PASS'; then
  exit 0
fi
echo "FAT32-ORACLE did not pass (sbcl exit $rc)" >&2
exit 1
