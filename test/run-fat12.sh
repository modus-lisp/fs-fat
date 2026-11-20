#!/bin/sh
# run-fat12.sh -- run test/fat12-test.lisp: FAT12 and FAT16 against mtools,
# mkfs.vfat and fsck.fat.  Exit status 0 only if it prints FAT12 PASS.
set -u
ROOT=$(cd "$(dirname "$0")/.." && pwd)
WORK=$(mktemp -d) || exit 1
trap 'rm -rf "$WORK"' EXIT
${SBCL:-sbcl} --noinform --non-interactive \
  --eval "(defvar *root* \"$ROOT/\")" --eval "(defvar *work* \"$WORK/\")" \
  --load "$ROOT/test/fat12-test.lisp" 2>&1 | grep -v "^;\|^$" | tee "$WORK/log"
grep -q "^FAT12 PASS" "$WORK/log"
