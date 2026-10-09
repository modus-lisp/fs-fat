;;;; fs-fat.asd -- ASDF system definition for the FAT32 and exFAT libraries.
;;;;
;;;;   (asdf:load-system "fs-fat")
;;;;
;;;; The package FS-FAT is defined in lib/fat32.lisp, which must load before
;;;; lib/exfat.lisp, hence :serial.  The test harnesses under test/ are not part of
;;;; this system: they need host tools (mkfs, fsck, mtools) and are run by the
;;;; scripts in test/.

(defsystem "fs-fat"
  :description "Pure Common Lisp FAT32 and exFAT readers and writers, with a device layer for crash testing."
  :author "ynniv"
  :version "0.1.0"
  :serial t
  :components ((:file "lib/fat32")
               (:file "lib/exfat")))
