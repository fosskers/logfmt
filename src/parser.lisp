(in-package :logfmt)

;; --- Static Parsers --- ;;

(defparameter +equal+      (p:char #\=))
(defparameter +skip-space+ (p:consume #'p:space?))
(defparameter +value-end+  (p:alt (p:sneak #\space) #'p:eof))
(defparameter +quote+      (p:char #\"))

;; --- Parsers --- ;;

(defun parse (s)
  "Parse a single log line into a Hash Table."
  (p:parse (<* #'pairs #'p:eof) s))

#+nil
(parse "level=info msg=\"Hello there!\" foo=3 bar=ok")

(defun safe-parse (s)
  "Parse a single log line, but yields NIL if parsing failed.
Can be used to skip garbage lines in a file that don't conform to logfmt syntax."
  (multiple-value-bind (res next) (funcall (<* #'pairs #'p:eof) (p:in s))
    (declare (ignore next))
    (when (p:ok? res)
      res)))

#+nil
(safe-parse "ts=2025-08-08T13:42:42.148 zzz level=info msg=foo run_id=ox_joKSkGaSYZ_sOL-kbU exec_name=hoods")

(defun pairs (offset)
  "Parse multiple key-value pairs into a Hash Table."
  (p:fmap (lambda (list)
            (let ((ht (make-hash-table :test #'equal)))
              (dolist (pair list)
                (setf (gethash (car pair) ht) (cadr pair)))
              ht))
          (funcall (p:sep-end1 +skip-space+ #'pair) offset)))

#+nil
(p:parse #'pairs "a=1 b=hi c=2")

#+nil
(parse "ts=2025-08-08T13:42:42.148 level=info msg=foo run_id=ox_joKSkGaSYZ_sOL-kbU exec_name=hoods")

#+nil
(parse "msg=\"foo bar\" took=100")

#+nil
(parse "ts=2025-08-08T13:42:42.148 level=info msg=\"Skipping pushing to database, as config_json_path, unique_id_func, and/or database_handler is None.\" run_id=ox_joKSkGaSYZ_sOL-kbU exec_name=hoods")

;; TODO: 2025-08-12 Avoid the list allocation if possible.
(defun pair (offset)
  "A key-value pair."
  (funcall (<*> #'key (*> +equal+ #'value)) offset))

#+nil
(p:parse #'pair "msg=foo")
#+nil
(p:parse #'pair "msg=4")

(defun key (offset)
  "The key portion of a single key-value pair."
  (funcall (p:take-while1 (lambda (c) (and (not (char= c #\=))
                                           (not (char= c #\space)))))
           offset))

#+nil
(p:parse #'key "msg=foo")

(defun value (offset)
  "The value portion of a single key-value pair."
  (funcall (p:alt (<* #'p:unsigned +value-end+)
                  #'quoted
                  (p:take-while (lambda (c) (not (char= c #\space)))))
           offset))

#+nil
(p:parse (*> #'key (p:char #\=) #'value) "msg=foo")

(defun quoted (offset)
  "Parser: A quoted string that may contain spaces, etc."
  (funcall (p:between +quote+
                      (p:take-while1 (lambda (c) (not (char= c #\"))))
                      +quote+)
           offset))

#+nil
(p:parse #'quoted "\"foo bar\"")
