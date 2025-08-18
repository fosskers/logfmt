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

(defun direct-child? (parent chain)
  (string= parent (car (last (butlast chain)))))

(defun id (entry)
  "The ID of a particular entry."
  (-> entry (getf :chain) last car))

(defun build-tree (curr rest)
  (let ((id (id curr)))
    (list :took (getf curr :took)
          :chain (getf curr :chain)
          :children (t:transduce
                     (t:comp (t:filter (lambda (other) (direct-child? id (getf other :chain))))
                             (t:map (lambda (child)
                                      (let ((child-id (id child)))
                                        (build-tree child (t:transduce
                                                           (t:filter (lambda (other) (direct-child? child-id (getf other :chain))))
                                                           #'t:cons rest))))))
                     #'t:cons rest))))

(defun descend (tree)
  "Recursively descend through the tree, summing and subtracting `took' weights as necessary."
  (if (null (getf tree :children))
      (let ((took (getf tree :took)))
        (values took (list (list :took took :chain (getf tree :chain)))))
      (destructuring-bind (acc . children-took)
          (t:transduce
           #'t:pass
           (t:fold (lambda (pair child)
                     (destructuring-bind (acc . total-took) pair
                       (multiple-value-bind (child-took v) (descend child)
                         (cons (append v acc) (+ total-took child-took)))))
                   (cons '() 0))
           (getf tree :children))
        (let ((this-took (- (getf tree :took) children-took)))
          (values (+ this-took children-took)
                  (cons (list :took this-took :chain (getf tree :chain))
                        acc))))))

#+nil
(let* ((total (-<>> (entries-from-file #p"tests/logs.txt")
                    (sort <> #'string< :key (lambda (plist) (getf plist :id)))
                    (t:transduce (t:comp (t:group-by (lambda (plist) (getf plist :id)))
                                         (t:map (lambda (list)
                                                  ;; FIXME: Still only getting the first one here. Need more.
                                                  (car (sort list #'> :key (lambda (plist) (getf plist :took)))))))
                                 #'t:cons)))
       (parents (make-hash-table :test #'equal))
       (names (make-hash-table :test #'equal)))
  (dolist (plist total)
    (setf (gethash (getf plist :id) parents) (getf plist :parent))
    (setf (gethash (getf plist :id) names) (getf plist :name)))
  (let* ((chained (t:transduce (t:map (lambda (entry)
                                        (list :took (getf entry :took)
                                              :chain (id-hierarchy parents (getf entry :id)))))
                               #'t:cons total))
         (sorted (sort chained #'< :key (lambda (list) (length (getf list :chain)))))
         (tree (build-tree (car sorted) (cdr sorted))))
    (multiple-value-bind (took chains) (descend tree)
      (let* ((lines (t:transduce
                     (t:map (lambda (entry)
                              (let ((id-chain (t:transduce (t:comp (t:map (lambda (pid) (gethash pid names)))
                                                                   (t:intersperse ";")
                                                                   #'t:flatten)
                                                           #'t:string (getf entry :chain))))
                                (format nil "~a ~a" id-chain (getf entry :took)))))
                     #'t:cons chains))
             #+nil
             (sorted (sort lines #'string<)))
        chains
        #+nil
        (with-open-file (stream #p"FLAMES.txt" :direction :output :if-exists :supersede)
          (t:transduce #'t:pass (t:for (lambda (line) (write-line line stream))) lines))))))

;; TODO: For each entry:
;;
;; - Grab its id-chain and make them all names.
;; - Grab its `took' and append it to the chain string.
;; - Yield a final list of all results. That's the flamegraph?
;;
;; No you still need to do subtracting.
