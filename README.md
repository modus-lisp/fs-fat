# fs-fat

FAT32 and exFAT readers and writers in pure Common Lisp. No FFI, no mounting, no
root: a volume is a byte vector, or a device you supply. The package is `FS-FAT`.

```lisp
(asdf:load-system "fs-fat")
(in-package :cl-user)
(use-package :fs-fat)

(let ((v (fat32-open (read-partition-bytes))))   ; (unsigned-byte 8) vector
  (fat32-read-file v "/docs/readme.txt")         ; fresh byte vector, or NIL
  (fat32-write-file v "/docs/new.txt" #(1 2 3))
  (fat32-delete v "/docs/old.txt")
  (fat32-rename v "/docs/new.txt" "/docs/final.txt"))
```

Writes change the byte vector in place, so save it afterwards. exFAT has the same
shape with `exfat-open`, `exfat-read-file`, `exfat-write-file`, and so on.

## API

| Function | Returns |
|---|---|
| `fat32-open bytes &optional device` | a volume; an error if `bytes` is not FAT32 |
| `fat32-lookup v path` | the entry at `path`, or `NIL` |
| `fat32-directory v path` | the entries of the directory at `path`, or `NIL` |
| `fat32-read-file v path` | the file's contents, or `NIL` |
| `fat32-write-file v path bytes` | writes (creating or replacing) a file |
| `fat32-mkdir`, `fat32-delete`, `fat32-rename` | directory and name operations |
| `fat32-sync v` | a write barrier: everything written so far is durable before anything after it |

Paths are `/`-separated; `""` and `"/"` name the root. An entry is
`(NAME DIRP SIZE FIRST-CLUSTER)`. Lookup is case-insensitive, as FAT is.

The `exfat-*` functions take the same arguments. Their entries are
`(NAME DIRP SIZE FIRST-CLUSTER CONTIGUOUS FIRST-SLOT COUNT)`.

## Devices

A volume can sit on a device instead of a byte vector:
`(make-fat32-device view &optional write-fn flush-fn)`, then `fat32-open-device`
or `(fat32-open bytes device)`. The device layer is what the crash tests use: it
records every write and flush as a numbered event, and can simulate a power loss
at any event, keeping only some of the writes that were not yet flushed
(`device-crash-writes`). `device-events` and `device-dead-p` report what happened.

Writes are sector-atomic, and the writers order them so that an interrupted
operation leaves either the old or the new state, with at most leaked clusters
(see the header comments of `lib/fat32.lisp` and `lib/exfat.lisp`).

## Scope

- **FAT32 only.** FAT12 and FAT16 are not supported.
- **Long names.** FAT long names are read and written as UTF-16, including
  surrogate pairs, and up to 255 units. Lookup folds case through the default
  up-case table.
- **exFAT.** One FAT is required; TexFAT volumes are refused. Names are limited to
  210 UTF-16 units. Files and directories with the `NoFatChain` flag (contiguous)
  are read, but writes always produce FAT chains, and contiguous directories
  cannot grow.
- **Memory.** A volume is held in memory as a byte vector, or read and written through a device.

## Requirements

Common Lisp with the standard library only. The package loads as plain source
with `load`, or through ASDF (`fs-fat.asd`), which loads `lib/fat32.lisp` before
`lib/exfat.lisp`.

## Tests

The tests in `test/` check the libraries against real tools. They run in user mode,
and they need the tools on `PATH`, or named by environment variables:

| Runner | Checks | Needs |
|---|---|---|
| `test/run-fat32.sh` | reads `mkfs.vfat` volumes | `mkfs.vfat`, `fsck.fat` |
| `test/run-fat32-write.sh` | writes, against a host mirror and `mtools` | `mkfs.vfat`, `fsck.fat`, `mtools` |
| `test/run-fat32-ops.sh` | random operations, checked by model and tools | the above |
| `test/run-fat32-oracle.sh` | random trees and fuzzed images, against `fsck.fat` | the above |
| `test/run-fat32-crash.sh` | crash at every write and flush of a sequence | the above |
| `test/run-exfat-write.sh` | writes, checked by `fsck.exfat` | `MKFS_EXFAT`, `FSCK_EXFAT` |
| `test/run-exfat-ops.sh` | random operations, checked by model and `fsck.exfat -n` | the above |
| `test/run-exfat-crash.sh` | crash sequences on exFAT | the above |
| `test/run-unicode-names.sh` | name handling across FAT and exFAT | `mtools`, the exFAT tools for the exFAT half |
| `test/run-volume-samples.sh` | reads two volumes made by other tools (Debian forensics samples) | network, `curl`, `xz`, `dpkg-deb`, the exFAT tools |

The exFAT tools are `mkfs.exfat` and `fsck.exfat` from exfatprogs. The `test/`
harnesses use `sb-ext:run-program` to run these tools. That is the only
implementation-specific call, and it is confined to `test/fat32-oracle.lisp`.
The random-operation runners take their seeds as arguments, so a failing run can be repeated.
