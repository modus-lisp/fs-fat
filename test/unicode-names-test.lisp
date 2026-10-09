;;;; test/unicode-names-test.lisp -- UTF-16 names: the encoding helpers, FAT names with
;;;; characters above U+FFFF, and exFAT name lengths in UTF-16 units.  Portable CL.
;;;;
;;;; The runner (test/run-unicode-names.sh) defines before loading this file:
;;;;   *uni-lib-fat*   lib/fat32.lisp        *uni-lib-exfat*  lib/exfat.lisp
;;;;   *uni-image*     an empty FAT32 image (rewritten in place)
;;;;   *uni-exfat-image*  an empty exFAT image, or NIL to leave the exFAT checks out
;;;;   *uni-names*     a file the names are written to, one per line, as mtools should
;;;;                   show them.  mtools cannot show a character above U+FFFF: it
;;;;                   shows each of its two UTF-16 units as an underscore, so the
;;;;                   expected names write each such character as "__".

(load *uni-lib-fat*)
(use-package :fs-fat)
(load *uni-lib-exfat*)

(defvar *uni-fails* 0)
(defvar *uni-exfat-image* nil)

(defun uni-check (ok label)
  (unless ok
    (incf *uni-fails*)
    (format t "FAIL: ~a~%" label))
  ok)

(defun uni-char (code) (string (code-char code)))

(defun uni-corpus ()
  (list (format nil "caf~a.txt" (code-char 233))
        (format nil "~a~a~a.txt" (code-char #x65e5) (code-char #x672c) (code-char #x8a9e))
        (format nil "smile~a.txt" (code-char #x1F600))
        (format nil "a~ab~ac.txt" (code-char #x1F600) (code-char #x1F4A9))
        (format nil "~a.txt" (make-string 35 :initial-element (code-char #x1F680)))
        (format nil "~a~a~a.txt" (code-char #xC0) (code-char #xC9) (code-char #xCE))
        (format nil "mixed ~a 2.txt" (code-char #x10FFFF))
        (format nil "~a~a~a.txt" (code-char #x393) (code-char #x3B1) (code-char #x3B2))
        (format nil "~a~a~a.txt" (code-char #x41F) (code-char #x440) (code-char #x438))))

;;; ---- the encoding ---------------------------------------------------------------------

(uni-check (equal (fs-fat::%utf16-encode "a") '(97)) "ASCII is one unit")
(uni-check (equal (fs-fat::%utf16-encode (string (code-char #x1F600))) '(#xD83D #xDE00))
           "U+1F600 is the surrogate pair D83D DE00")
(uni-check (equal (fs-fat::%utf16-encode (string (code-char #x10FFFF))) '(#xDBFF #xDFFF))
           "U+10FFFF is the surrogate pair DBFF DFFF")
(uni-check (= (length (fs-fat::%utf16-encode (format nil "~a~a" (code-char #x1F600) (code-char #x1F600))))
              4)
           "two supplementary characters are four units")

(dolist (name (uni-corpus))
  (uni-check (string= (fs-fat::%utf16-decode (fs-fat::%utf16-encode name)) name)
             (format nil "round trip of ~s" name)))

(uni-check (= (char-code (char (fs-fat::%utf16-decode '(#xD83D #xDE00)) 0)) #x1F600)
           "a surrogate pair decodes to one character")
(uni-check (= (length (fs-fat::%utf16-decode '(#xD83D #xDE00))) 1)
           "a surrogate pair decodes to a single character, not two")
(uni-check (= (char-code (char (fs-fat::%utf16-decode '(#xD83D 97)) 0)) #xD83D)
           "a lone high surrogate is kept as its own character")

;;; ---- FAT: names above U+FFFF, through the reader and mtools ----------------------------

(defun uni-read (path)
  (with-open-file (s path :element-type '(unsigned-byte 8))
    (let ((v (make-array (file-length s) :element-type '(unsigned-byte 8))))
      (read-sequence v s)
      v)))

(defun uni-write (path bytes)
  (with-open-file (s path :direction :output :element-type '(unsigned-byte 8)
                          :if-exists :supersede)
    (write-sequence bytes s)))

(let* ((bytes (uni-read *uni-image*))
       (v (fat32-open bytes))
       (names (uni-corpus)))
  (dolist (name names)
    (fat32-write-file v name (make-array 10 :element-type '(unsigned-byte 8) :initial-element 65)))
  (uni-write *uni-image* bytes)
  (let* ((again (fat32-open (uni-read *uni-image*)))
         (listed (sort (mapcar #'first (fat32-directory again "")) #'string<)))
    (uni-check (equal listed (sort (copy-list names) #'string<))
               "the reader lists the names it wrote, exactly")
    ;; FAT lookup is case-insensitive for any letter in the default up-case table
    (uni-check (fat32-lookup again (concatenate 'string "CAF" (uni-char #xC9) ".TXT"))
               "FAT lookup finds the accented name in capitals")
    (uni-check (fat32-lookup again (concatenate 'string (uni-char #xE0) (uni-char #xE9)
                                                (uni-char #xEE) ".txt"))
               "FAT lookup finds the capitals from lower case: Latin-1")
    (uni-check (fat32-lookup again (concatenate 'string (uni-char #x3B3) (uni-char #x3B1)
                                                (uni-char #x3B2) ".txt"))
               "FAT lookup finds the Greek capitals from lower case")
    (uni-check (fat32-lookup again (concatenate 'string (uni-char #x43F) (uni-char #x420)
                                                (uni-char #x438) ".TXT"))
               "FAT lookup finds the Cyrillic name from the other case")
    (with-open-file (s *uni-names* :direction :output :if-exists :supersede)
      (dolist (n (sort (copy-list names) #'string<))
        (loop for ch across n
              do (if (< (char-code ch) #x10000)
                     (write-char ch s)
                     (write-string "__" s)))
        (terpri s)))))



;;; ---- exFAT: lengths in units, case through the up-case table, names above U+FFFF ---

(when *uni-exfat-image*
  (let* ((bytes (uni-read *uni-exfat-image*))
         (v (exfat-open bytes))
         (name (concatenate 'string "a" (uni-char #x1F600)))
         (set (fs-fat::%ex-set-bytes v (concatenate 'string "a" (make-string 20 :initial-element (code-char #x1F600)))
                             nil 0 0 nil))
         (accented (concatenate 'string "caf" (uni-char #xE9) ".txt"))
         (upper (concatenate 'string "CAF" (uni-char #xC9) ".TXT"))
         (emoji (concatenate 'string "smile" (uni-char #x1F600) ".txt")))
    (declare (ignorable name))
    (uni-check (= (aref set 35) 41)
               "exFAT NameLength is the count in units: 1 + 20 pairs = 41")
    (uni-check (= (floor (length set) 32) 5)
               "41 units need three name entries, so five entries in the set")
    (exfat-write-file v accented (make-array 4 :element-type '(unsigned-byte 8) :initial-element 1))
    (uni-check (exfat-lookup v upper)
               "a name in capitals finds the accented name through the up-case table")
    (exfat-write-file v upper (make-array 6 :element-type '(unsigned-byte 8) :initial-element 2))
    (uni-check (= (length (exfat-directory v "")) 1)
               "a case variant replaces its entry rather than adding one")
    (exfat-write-file v emoji (make-array 3 :element-type '(unsigned-byte 8) :initial-element 3))
    (uni-check (equal (mapcar #'first (exfat-directory v ""))
                      (list upper emoji))
               "a name above U+FFFF is read back exactly, next to the accented one")
    ;; FAT's built-in table is the one exFAT volumes carry: they agree on every unit
    (uni-check (loop for c from 0 below 65536
                     always (= (fs-fat::%fat-upcase-unit c) (fs-fat::%ex-upcase v c)))
               "FAT's up-case table matches the exFAT volume's on every unit")
    (uni-write *uni-exfat-image* bytes)))

(format t "UNICODE-NAMES ~a failures=~a~%"
        (if (= *uni-fails* 0) "PASS" "FAIL") *uni-fails*)
