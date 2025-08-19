(defpackage logfmt/flamegraph
  (:use :cl :arrow-macros)
  (:local-nicknames (#:t #:transducers)
                    (#:b64 #:cl-base64)))

(in-package :logfmt/flamegraph)

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
  "Is some entry, represented by its call chain, the direct child of a given parent?"
  (string= parent (car (last (butlast chain)))))

(defun id (entry)
  "The ID of a particular entry."
  (-> entry (getf :chain) last car))

(defun build-tree (curr rest)
  "Given all known call chains, recursively form a tree of their call structure."
  (let ((id (id curr)))
    (list :took (getf curr :took)
          :chain (getf curr :chain)
          :children (t:transduce
                     (t:comp (t:filter (lambda (other) (direct-child? id (getf other :chain))))
                             (t:map (lambda (child) (build-tree child rest))))
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
        ;; HACK: 2025-08-19 For large log files I'm still getting a total
        ;; negative time for the top-level script, but only by a few hundred
        ;; milliseconds. Rounding error among the blocks? Not sure. For now,
        ;; rejecting anything that reports a negative time is a workaround.
        (let ((this-took (max 0 (- (getf tree :took) children-took))))
          (values (+ this-took children-took)
                  (cons (list :took this-took :chain (getf tree :chain))
                        acc))))))

(defun sanitize-entries (entries)
  "Strategically reassign IDs so that `took' messages within the same binary are
properly unique."
  (t:transduce
   (t:comp
    (t:group-by (lambda (plist) (getf plist :id)))
    (t:map (lambda (list)
             (let* ((sorted (sort list #'> :key (lambda (plist) (getf plist :took))))
                    (parent (car sorted))
                    (kids   (cdr sorted)))
               (cons parent
                     (t:transduce
                      (t:comp #'t:enumerate
                              (t:map (lambda (pair)
                                       (let* ((i (car pair))
                                              (child (cdr pair))
                                              (name (t:transduce (t:map (lambda (c) (if (char= c #\space) #\_ c)))
                                                                 #'t:string (getf child :msg))))
                                         (list :name name
                                               :msg  (getf child :msg)
                                               :took (getf child :took)
                                               :parent (getf child :id)
                                               :id (b64:string-to-base64-string (format nil "~d~a~a" i (getf child :id) name)))))))
                      #'t:cons kids)))))
    #'t:concatenate)
   #'t:cons entries))

(defun render-entries (names entries)
  "Given a lookup of all IDs to their original names, render each `entry' into its
flamegraph-compatible format."
  (t:transduce
   (t:map (lambda (entry)
            (let ((id-chain (t:transduce (t:comp (t:map (lambda (pid) (gethash pid names)))
                                                 (t:intersperse ";")
                                                 #'t:flatten)
                                         #'t:string (getf entry :chain))))
              (format nil "~a ~a" id-chain (getf entry :took)))))
   #'t:cons entries))

(defun flamegraph (in-path)
  "Read a logfmt file and produce flamegraph entries."
  (let* ((entries (-<>> (entries-from-file in-path)
                        (sort <> #'string< :key (lambda (plist) (getf plist :id)))
                        (sanitize-entries)))
         (parents (make-hash-table :test #'equal))
         (names (make-hash-table :test #'equal)))
    (dolist (plist entries)
      (setf (gethash (getf plist :id) parents) (getf plist :parent))
      (setf (gethash (getf plist :id) names) (getf plist :name)))
    (let* ((chained (t:transduce (t:map (lambda (entry)
                                          (list :took (getf entry :took)
                                                :chain (id-hierarchy parents (getf entry :id)))))
                                 #'t:cons entries))
           (sorted (sort chained #'< :key (lambda (list) (length (getf list :chain)))))
           ;; NOTE: The item with the shortest chain (presumably length 1)
           ;; should be definition be the item that took the longest. In other
           ;; words, the top-level program. Hence the `sort' call above and the
           ;; `car' call below.
           (tree (build-tree (car sorted) (cdr sorted))))
      (multiple-value-bind (took chains) (descend tree)
        (declare (ignore took))
        (render-entries names chains)))))

#+nil
(flamegraph #p"tests/logs.txt")

(defun write-entries (out-path entries)
  (with-open-file (stream out-path :direction :output :if-exists :supersede)
    (t:transduce #'t:pass (t:for (lambda (line) (write-line line stream))) entries)))

#+nil
(write-entries #p"FLAMES.txt" (flamegraph #p"tests/logs.txt"))
