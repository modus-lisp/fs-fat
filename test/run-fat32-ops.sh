#!/bin/sh
# run-fat32-ops.sh -- random operations on a mkfs.vfat volume, checked three ways:
# the in-memory model, lib/fat32.lisp's reader, and mtools' export of the image.
# fsck.fat -n must accept the image at every check.  User mode: no mount, no root.
#
#   test/run-fat32-ops.sh [SEEDS] [FIRST_SEED] [COUNT] [EVERY]
#
#   SEEDS       consecutive seeds to run              (default 3)
#   FIRST_SEED  the first seed                        (default 1)
#   COUNT       operations per seed                   (default 200)
#   EVERY       check the volume every N operations   (default 10)
#   KEEP=DIR    keep each seed's image and exports in DIR
#
# A seed fixes everything, so re-running with the same arguments reproduces it.
# Exit status 0 only if the run prints FAT32-OPS PASS.

set -u

ROOT=$(cd "$(dirname "$0")/.." && pwd)
SBCL=${SBCL:-sbcl}
SEEDS=${1:-3}
FIRST=${2:-1}
COUNT=${3:-200}
EVERY=${4:-10}

WORK=$(mktemp -d) || exit 1
if [ -n "${KEEP:-}" ]; then
  mkdir -p "$KEEP"
  WORKDIR=$(cd "$KEEP" && pwd)/run-$$
  mkdir -p "$WORKDIR"
else
  WORKDIR=$WORK/work
  mkdir -p "$WORKDIR"
fi
trap 'rm -rf "$WORK"' EXIT

cat > "$WORK/run.lisp" <<EOF
(defvar *oracle-root* "$ROOT/")
(defvar *oracle-workdir* "$WORKDIR")
(defvar *ops-first-seed* $FIRST)
(defvar *ops-seeds* $SEEDS)
(defvar *ops-count* $COUNT)
(defvar *ops-every* $EVERY)
(load "$ROOT/test/fat32-ops.lisp")
EOF

out=$("$SBCL" --script "$WORK/run.lisp" 2>&1)
rc=$?
echo "$out"
if [ -n "${KEEP:-}" ]; then
  echo "kept in: $WORKDIR"
fi

if [ "$rc" -eq 0 ] && echo "$out" | grep -q '^FAT32-OPS PASS'; then
  exit 0
fi
echo "FAT32-OPS did not pass (sbcl exit $rc)" >&2
exit 1
