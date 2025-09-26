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
(parse "msg=\"foo \\\" bar\" took=100")

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
                      (sliding-take (lambda (a b)
                                      (cond ((and (char= a #\\)
                                                  (char= b #\"))
                                             (values :two #\"))
                                            ((not (char= a #\")) (values :one a)))))
                      +quote+)
           offset))

#+nil
(p:parse #'quoted "\"foo bar\"")
#+nil
(p:parse #'quoted "\"foo \\\" bar\"")

(defun sliding-take (f)
  (lambda (offset)
    (declare (optimize (speed 3) (safety 0)))
    (let* ((s (make-array 16 :element-type 'character :adjustable t :fill-pointer 0))
           (keep (loop :with i fixnum := offset
                       :while (< i p::*input-length*)
                       :do (let ((a (schar p::*input* i))
                                 (b (if (< i (1- p::*input-length*))
                                        (schar p::*input* (1+ i))
                                        #\Nul)))
                             (multiple-value-bind (kw c) (funcall f a b)
                               (case kw
                                 (:one
                                  (incf i)
                                  (vector-push-extend c s))
                                 (:two
                                  (incf i 2)
                                  (vector-push-extend c s))
                                 (t (return (- i offset))))))
                       :finally (return (- i offset))))
           (next (p::off keep offset)))
      (values s next))))

#+nil
(p:parse (sliding-take (lambda (a b)
                         (cond ((and (char= a #\\)
                                     (char= b #\"))
                                (values :two #\"))
                               (t (values :one a)))))
         "Hello \\\" there!")
