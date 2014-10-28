(in-package :org-sampler)

(defvar *no-edit-message* "DO NOT EDIT --- generated from source code.")

(defun write-fn-file (sym path)
  (with-open-file (stream path :direction :output :if-exists :supersede)
    (format stream "# ~a~%" *no-edit-message*)

    (flet ((gfll (fn)
             #+allegro (mop:generic-function-lambda-list fn)
             #+sbcl (sb-mop:generic-function-lambda-list fn)
             #-(or allegro sbcl)
             (error "Don't know how to call the MOP on this Lisp")))
      (let ((fn (symbol-function sym))
            (doc-string (documentation sym 'function)))

        (iter (for i from (length doc-string) downto 1)
              (when (eql (aref doc-string (- i 1)) #\Newline)
                (setf doc-string (concatenate 'string (subseq doc-string 0 i)
                                              "  " (subseq doc-string i)))))

        (cond
          ((typep (symbol-function sym) 'generic-function)
           (let ((fn-call (format nil "(~a~{ ~a~})" sym (gfll fn))))
             (format stream "- =~a= ~a\\hspace*{\\fill} /Generic function/"
               fn-call (cond ((> (length fn-call) 42) "\\\\") (t "")))))
          (t
           (let ((fn-call (symbol-name sym)))
             (format stream "- =~a= ~a\\hspace*{\\fill} /~a/"
               fn-call
               (cond ((> (length fn-call) 48) "\\\\") (t ""))
               (cond
                 ((special-operator-p sym) "Special operator")
                 ((macro-function sym) "Macro")
                 (t "Function"))))))
        (when doc-string
          (format stream "~%~%  ~a~%~%" doc-string))))))

(defun write-type-file (sym path)
  (with-open-file (stream path :direction :output :if-exists :supersede)
    (format stream "# ~a~%" *no-edit-message*)

    (let ((doc-string (documentation sym 'type)))
      (format stream "- =~a= \\hspace*{\\fill} /Type/" (symbol-name sym))
      (when doc-string (format stream "~%~%  ~a~%~%" doc-string)))))

(defun write-variable-file (sym path)
  (with-open-file (stream path :direction :output :if-exists :supersede)
    (format stream "# ~a~%" *no-edit-message*)

    (let ((doc-string (documentation sym 'variable)))
      (format stream "- =~a= \\hspace*{\\fill} /~:[Variable~;Constant~]/"
        (symbol-name sym) (constantp sym))
      (when doc-string (format stream "~%~%  ~a~%~%" doc-string)))))

(defun set-index-usage (index-acc symbol usage path)
  "The =index-acc= hash table builds up path information for an index."

  ;; Pull the hash table for the SYMBOL, creating and caching one if
  ;; needed.
  (let ((symbol-hash (gethash symbol index-acc)))
    (unless symbol-hash
      (setf symbol-hash (make-hash-table :test 'eq)
            (gethash symbol index-acc) symbol-hash))

    ;; Store the path for this use of the symbol
    (setf (gethash usage symbol-hash) path))
  (values))

(defun write-symbol-files (symbol directory-path
                           &key index-acc always-disambiguate)
  "Writes Org-mode files documenting the uses of the given =symbol=."
  (let* ((use-name (string-downcase (symbol-name symbol)))
         (is-fn (fboundp symbol))
         (is-type (find-class symbol nil))
         (is-variable (boundp symbol))
         (uses (iter (for bool in (list is-fn is-type is-variable))
                     (when bool (summing 1))))
         (disambiguate (or always-disambiguate (> uses 1))))

    (iter (for i from 0 below (length use-name))
          (when (or (eql (aref use-name i) #\/)
                    (eql (aref use-name i) #\*))
            (setf (aref use-name i) #\_)))

    (when is-fn
      (let ((path (merge-pathnames (concatenate 'string
                                     use-name (if disambiguate "__fn" "")
                                     ".org")
                                   directory-path)))
        (write-fn-file symbol path)
        (when index-acc (set-index-usage index-acc symbol :fn path))))

    (when is-type
      (let ((path (merge-pathnames
                   (concatenate 'string
                     use-name (if disambiguate "__type" "") ".org")
                   directory-path)))
        (write-type-file symbol path)
        (when index-acc (set-index-usage index-acc symbol :type path))))

    (when is-variable
      (let ((path (merge-pathnames
                   (concatenate 'string
                     use-name (if disambiguate "__variable" "") ".org")
                   directory-path)))
        (write-variable-file symbol path)
        (when index-acc (set-index-usage index-acc symbol :variable path))))))

(defun write-package-files (package &key all system path index
                                      (index-acc (make-hash-table :test 'eq)))
  "Documents a package by writing an Org file for each defined symbol.
- The =package= argument should be a package specifier.
- If the =all= keyword argument is given and is non-nil, then all symbols in
  the package should be documented, instead of just exported symbols.
- The =path= argument gives the directory where the files should be written.
  This directory will be created if it does not exist.
- A relative =path= is resolved relative to the location of the ASDF file
  defining the given =system=."
  (unless path (setf path #p"./"))
  (when system (setf path (asdf:system-relative-pathname system path)))

  (ensure-directories-exist path)
  (cond (all (do-symbols (sym package)
               (when (eq (symbol-package sym) (find-package package))
                 (write-symbol-files sym path :index-acc index-acc))))
        (t (do-external-symbols (sym package)
             (write-symbol-files sym path :index-acc index-acc))))
  (when index
    (format t "Writing index for package.~%")
    (write-index-org index-acc path index)))

(defun write-packages (packages &key default-all default-system
                                  (default-path "./")
                                  (package-extension t) (extension-downcase t)
                                  index (index-system default-system)
                                  (index-acc (make-hash-table :test 'eq)))
  "Document several packages by making a call to =write-package-files= for each.
The =packages= argument is a list giving a specification of the packages to be
documented.  Each element can be either a package designator or a list whose
first element is a package designator and other elements are keyword arguments
accepted by =write-package-files=.  These keywords will be used for the call
to =write-package-files= for that package.
The =default-all=, =default-system=, and =default-path= arguments give the
default arguments for the calls to =write-package-files=.
If =package-extension= is non-nil (its default is =t=), then whenever a package
spec does not give an explicit path, it should use a subdirectory of the default
path whose name is taken from the package.  If =extension-downcase= is non-nil
\(its default is =t=), then the package name is converted to lower-case for this
extension."
  (iter (for package-spec in packages)
        (when (symbolp package-spec)
          (setf package-spec (list package-spec)))
        (destructuring-bind (package &key (all default-all)
                                          (system default-system)
                                          (path default-path path-supp-p)
                                          (subdir "./"))
            package-spec
          (cond
            (path-supp-p
             (when package-extension
               (let ((extension (package-name package)))
                 (when extension-downcase
                   (setf extension (string-downcase extension)))
                 (setf path (merge-pathnames (concatenate 'string extension "/")
                                             path)))))
            (t (setf path (merge-pathnames subdir path))))
          (write-package-files package :all all :system system :path path
                               :index-acc index-acc)))
  (when index
    (format t "Writing index for packages.~%")
    (write-index-org index-acc
                     (asdf:system-relative-pathname index-system default-path)
                     index))
  (values))

(defun write-index-org (index-acc directory-path index-file-name)
  (unless (stringp index-file-name) (setf index-file-name "index"))
  (let ((path (merge-pathnames (concatenate 'string index-file-name ".org")
                               directory-path))
        (names (sort (iter (for (key val) in-hashtable index-acc)
                           (declare (ignore val))
                           (collect key))
                     #'string< :key #'symbol-name)))
    (with-open-file (out path :direction :output :if-exists :supersede)
      (iter (for name in names)
            (format out "- ~a" name)
            (iter (for (usage path) in-hashtable (gethash name index-acc))
                  (for sep initially ":" then ",")
                  (format out "~a [[file:~a][~a]]" sep path usage))
            (format out ".~%"))))
  (values))
