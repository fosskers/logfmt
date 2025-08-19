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
        (let ((this-took (- (getf tree :took) children-took)))
          (values (+ this-took children-took)
                  (cons (list :took this-took :chain (getf tree :chain))
                        acc))))))

(defun simplify-name (name)
  "Reduce a name to just the initial character of each snake-case section."
  (t:transduce (t:map (lambda (s) (char s 0))) #'t:string (t::string-split name :separator #\_)))

#+nil
(simplify-name "cot_solid_mass")

#+nil
(let* ((total (-<>> (entries-from-file #p"tests/logs.txt")
                    (sort <> #'string< :key (lambda (plist) (getf plist :id)))
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
                                                                (name (concatenate 'string
                                                                                   (simplify-name (getf parent :name))
                                                                                   "_"
                                                                                   (t:transduce (t:map (lambda (c) (if (char= c #\space) #\_ c)))
                                                                                                #'t:string (getf child :msg)))))
                                                           (list :name name
                                                                 :msg  (getf child :msg)
                                                                 :took (getf child :took)
                                                                 :parent (getf child :id)
                                                                 :id (concatenate 'string "cgw-" (b64:string-to-base64-string (format nil "~d~a~a" i (getf child :id) name))))))))
                                        #'t:cons kids)))))
                      #'t:concatenate)
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
        (with-open-file (stream #p"FLAMES.txt" :direction :output :if-exists :supersede)
          (t:transduce #'t:pass (t:for (lambda (line) (write-line line stream))) lines))))))

