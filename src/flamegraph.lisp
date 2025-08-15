(defpackage logfmt/flamegraph
  (:use :cl :arrow-macros)
  (:local-nicknames (#:t #:transducers)))

(in-package :logfmt/flamegraph)

;; It might not matter that all you have to differentiate timing lines from the
;; same executable is the `msg'. The line that measures the runtime of the whole
;; program should always be the longest, and it would contain the times of the
;; other sub pieces. So, it's always (- longest rest). However, this assumes
;; that each callable thing is always providing its own total timing. This is
;; probably a safe assumption, or else each call (imagined as a function call)
;; won't report useful total timing data. The resulting flamegraph would be
;; inaccurate.
;;
;; Or, perhaps, the total program timing message should just have no `msg'. But
;; then does that makes the logs themselves less useful? Maybe, and it's also a
;; source of footgun. Somebody writing any old program that they want to time
;; would have to remember each time that a message can't be given for the
;; top-level.

(defun entries-from-file (path)
  "Read all perf-related messages from a logging file."
  (t:transduce (t:comp (t:filter-map #'logfmt:safe-parse)
                       (t:filter (lambda (ht) (gethash "took" ht)))
                       (t:map (lambda (ht) (list :name (gethash "exec_name" ht)
                                                 :msg (gethash "msg" ht)
                                                 :took (gethash "took" ht)
                                                 :id (gethash "run_id" ht)
                                                 :parent (gethash "parent_run_id" ht)))))
               #'t:cons path))

(defun id-hierarchy (ids id)
  "Given an ID and a mapping of all IDs to their parents, yield that ID's full chain."
  (labels ((recur (acc curr)
             (let ((parent (gethash curr ids)))
               (cond ((null parent) (cons curr acc))
                     (t (recur (cons curr acc) parent))))))
    (recur '() id)))

#+nil
(let* ((total (-<>> (entries-from-file #p"tests/logs.txt")
                    (sort <> #'string< :key (lambda (plist) (getf plist :id)))
                    (t:transduce (t:comp (t:group-by (lambda (plist) (getf plist :id)))
                                         (t:map (lambda (list)
                                                  (car (sort list #'> :key (lambda (plist) (getf plist :took)))))))
                                 #'t:cons)))
       (parents (make-hash-table :test #'equal))
       (names (make-hash-table :test #'equal))
       (test-id "NKMc98CL8aaeqi582R1tL"))
  (dolist (plist total)
    (setf (gethash (getf plist :id) parents) (getf plist :parent))
    (setf (gethash (getf plist :id) names) (getf plist :name)))
  (with-open-file (stream #p"FLAMES.txt" :direction :output :if-exists :supersede)
    (t:transduce (t:comp (t:map (lambda (entry)
                                  (let* ((id (getf entry :id))
                                         (id-chain (t:transduce (t:comp (t:map (lambda (pid) (gethash pid names)))
                                                                        (t:intersperse ";")
                                                                        #'t:flatten)
                                                                #'t:string (id-hierarchy parents id))))
                                    (format nil "~a ~a" id-chain (getf entry :took))))))
                 (t:for (lambda (line) (write-line line stream))) total)))

;; TODO: For each entry:
;;
;; - Grab its id-chain and make them all names.
;; - Grab its `took' and append it to the chain string.
;; - Yield a final list of all results. That's the flamegraph?
;;
;; No you still need to do subtracting.
