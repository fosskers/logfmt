(defpackage logfmt
  (:use :cl)
  (:import-from :parcom #:<*> #:<* #:*> #:<$)
  (:local-nicknames (#:p #:parcom))
  (:export #:parse #:safe-parse #:read-file-into-string)
  (:documentation "Parser for the logfmt logging style."))

(in-package :logfmt)

(declaim (ftype (function ((or string pathname)) (simple-array character (*))) read-file-into-string))
(defun read-file-into-string (path)
  "Read the contents of a file into a string."
  (with-open-file (stream path :direction :input :element-type 'character)
    (with-output-to-string (out)
      (loop :for c := (read-char stream nil :eof)
            :until (eq c :eof)
            :do (write-char c out)))))

#+nil
(read-file-into-string #p"tests/logs.txt")
