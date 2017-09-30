(in-package :org-sampler)

(defvar *no-edit-message* "DO NOT EDIT --- generated from source code."
  "Message printed at the top of ")
(defvar *index-writer* t
  "Should be left as =t=: used to indicate which outermost function API call should invoke =write-index-org=.")

(defparameter *generate-html* t
  "If non-nil, then an HTML file should be generated from each Org file.")
(defparameter *show-usage-header* t
  "Whether a header line for the usage should be written.")
(defparameter *show-package-header* t
  "Whether a header line for the package should be written.")
(defparameter *show-title* t
  "Whether an initial comment with the title should be written.")
(defparameter *section-level* nil
  "If non-nil, then generated Org mode with begin with the indicated level of section header giving the name and use of the definition. If =nil=, no section header is generated.")
(defparameter *latex-label-suffix* nil
  "If non-nil, then a LaTeX label definition will be generated for each symbol
with the given suffix.")

(defvar *standard-doctype-alist*
    '((function . "fn") (type . "type") (variable . "variable"))
  "Association list from symbols naming a =doc-type= to the string used to
disambiguate the generated files when needed.")

(defun write-org-file (sym path typ)
  (with-open-file (stream path :direction :output :if-exists :supersede)
    (format stream "# ~a~%" *no-edit-message*)
    (let ((type-name (case typ
                       ((function)
                        (cond
                          ((typep (symbol-function sym) 'generic-function)
                           "Generic function")
                          ((special-operator-p sym) "Special operator")
                          ((macro-function sym) "Macro")
                          (t "Function")))
                       ((variable) (if (constantp sym) "Constant" "Variable"))
                       ((type)     "Type")
                       (otherwise (string-capitalize (symbol-name typ)))))
          (doc-string (documentation* sym typ))
          (usage (when (and (eq typ 'function) (fboundp sym)
                            (typep (symbol-function sym) 'generic-function))
                   (flet ((gfll (fn)
                            #+allegro (mop:generic-function-lambda-list fn)
                            #+sbcl (sb-mop:generic-function-lambda-list fn)
                            #-(or allegro sbcl)
                            (error "Don't know how to call the MOP")))
                     (format nil "(~a~{ ~a~})"
                       sym (gfll (symbol-function sym)))))))
      (when *show-title*
        (format stream "#+TITLE: ~a =~a=~%" type-name sym))
      (when *latex-label-suffix*
        (format stream "#+LaTeX: \\label{~a~a}~%" sym *latex-label-suffix*))
      (when (and *section-level*
                 (numberp *section-level*) (> *section-level* 0))
        (iter (for i from 0 below *section-level*)
              (format stream "*"))
        (format stream " ~a =~a=~%" type-name sym))
      (cond
        ((and usage *show-usage-header* *show-package-header*)
         (format stream "- Package =~a=~%- Usage =~a=~%"
           (package-name (symbol-package sym)) usage))
        ((and *show-package-header*)
         (format stream "Package =~a=~%~%" (package-name (symbol-package sym))))
        ((and usage *show-usage-header*)
         (format stream "Usage =~a=~%~%" usage)))
      (when doc-string (format stream "~a~%~%" doc-string)))))

(defun write-symbol-files (symbol directory-path
                           &key index-acc always-disambiguate
                                (types *standard-doctype-alist*))
  "Writes Org-mode files (in the directory named by =directory-path=) documenting the uses of the given =symbol=.
#+begin_example
\(write-symbol-files SYMBOL DIRECTORY-PATH
  [ :index-acc HASH ] [ :always-disambiguate FLAG ] )
#+end_example
- The =index-acc= is a hash-table used to accumulate symbol references for an index page, or =nil= if no index data should be saved.
- This function will write a separate file for each /use/ of the symbol, disambiguating the file name where necessary with =__fn=, =__var= and so forth.  If =always-disambiguate= is non-nil, then these suffixes will /always/ be added to the names of the generated files, even when a symbol has only one usage."
  (let* ((use-name (string-downcase (symbol-name symbol)))
         (uses (iter (for (typ . suffix) in types)
                     (declare (ignore suffix))
                     (when (documentation* symbol typ) (summing 1))))
         (disambiguate (or always-disambiguate (> uses 1))))

    (iter (for i from 0 below (length use-name))
          (when (or (eql (aref use-name i) #\/)
                    (eql (aref use-name i) #\*))
            (setf (aref use-name i) #\_)))

    (iter (for (typ . suffix) in types)
          (when (documentation* symbol typ)
            (let ((path
                   (merge-pathnames (if disambiguate
                                      (concatenate 'string
                                        use-name "__" suffix ".org")
                                      (concatenate 'string use-name ".org"))
                                    directory-path)))
              (write-org-file symbol path typ)
              (when index-acc (set-index-usage index-acc symbol :fn path)))))))

(defun write-package-files (package &rest keyargs
                            &key all system path index
                              (types *standard-doctype-alist*)
                              (index-acc (make-hash-table :test 'eq))
                              show-package hoist-exported page-title
                              always-disambiguate
                              (section-level *section-level*)
                              (package-headers *show-package-header*)
                              (usage-headers *show-usage-header*)
                              (show-title *show-title*)
                              (latex-label-suffix *latex-label-suffix*))
  "Documents a package by writing an Org file for each defined symbol.
#+begin_example
\(write-package-files PACKAGE
                     [ :all FLAG ]
                     [ :system ASDF-SYSTEM-NAME ]
                     [ :path DIR ]
                     [ :index FLAG ]
                     [ :index-acc HASH-OR-NIL ]
                     [ :show-package FLAG ]
                     [ :hoist-exported FLAG ]
                     [ :page-title STRNIG ]
                     [ :always-disambiguate FLAG ]
                     [ :section-level NUMBER-OR-NIL ]
                     [ :package-headers FLAG ]
                     [ :usage-headers FLAG ]
                     [ :show-title FLAG ] )
#+end_example
- The =package= argument should be a package specifier.
- If the =all= keyword argument is given and is non-nil, then all symbols in
  the package should be documented, instead of just exported symbols.
- The =path= argument gives the directory where the files should be written.
  This directory will be created if it does not exist.
- A relative =path= is resolved relative to the location of the ASDF file
  defining the given =system=.
- When this function is called by another function in this package, =index-acc=
  will be a hash-table used to accumulate symbol references for the index that
  page will create.  When this function is called as an API from outside of this
  system, or if no index will be needed, then the argument should be left =nil=.
- The =show-package=, =hoist-exported=, and =page-title= keyword arguments
  direct the formatting of the index page which this routine should create when
  it is the entry call to this package.
  - If =show-package= is non-nil, then the symbol's package name will be
    dsplayed on any index page.
  - If =hoist-exported= in non-nil, then the list of symbols will be divided
    according to the package which exports each symbol, or by internal symbols
    of all packages.
  - If non-nil, =page-title= should be a string to be used as the page name."
  (format t "Path ~s~%" path)
  (unless path (setf path #p"./"))
  (when system (setf path (asdf:system-relative-pathname system path)))

  (ensure-directories-exist path)
  (let ((*index-writer* nil)
        (*section-level* section-level)
        (*show-title* show-title)
        (*show-usage-header* package-headers)
        (*show-package-header* usage-headers)
        (*standard-doctype-alist* types)
        (*latex-label-suffix* latex-label-suffix))
    (cond (all (do-symbols (sym package)
                 (when (eq (symbol-package sym) (find-package package))
                   (write-symbol-files sym path :index-acc index-acc
                                       :always-disambiguate always-disambiguate))))
          (t (do-external-symbols (sym package)
               (write-symbol-files sym path :index-acc index-acc
                                   :always-disambiguate always-disambiguate))))
    (when (and *index-writer* index)
      (format t "Writing index for package.~%")
      (write-index-org index-acc path index
                       :show-package show-package :hoist-exported hoist-exported
                       :title page-title
                       :always-disambiguate always-disambiguate))))

(defun write-packages (packages &rest keyargs
                       &key default-all default-system
                         (default-path "./")
                         (package-extension t) (extension-downcase t)
                         index (index-system default-system)
                         (types *standard-doctype-alist*)
                         always-disambiguate
                         (index-acc (make-hash-table :test 'eq))
                         show-package hoist-exported page-title
                         (section-level *section-level*)
                         (show-title *show-title*)
                         (package-headers *show-package-header*)
                         (usage-headers *show-usage-header*)
                         (latex-label-suffix *latex-label-suffix*))
  "Document several packages by making a call to =write-package-files= for each.
#+begin_example
\(write-packages PACKAGES
                [ :default-all FLAG ]
                [ :default-system ASDF-SYSTEM-NAME ]
                [ :default-path DIR ]
                [ :package-extension FLAG ]
                [ :extension-downcase FLAG ]
                [ :index FLAG ]
                [ :index-system ASDF-SYSTEM-NAME ]
                [ :index-acc HASH-TABLE ]
                [ :show-package FLAG ]
                [ :hoist-exported page-title FLAG ]
                [ :show-title FLAG ]
                [ :package-headers FLAG ]
                [ :usage-headers FLAG ] )
#+end_example
- The =packages= argument is a list giving a specification of the packages to be
  documented.  Each element can be either a package designator or a list whose
  first element is a package designator and other elements are keyword arguments
  accepted by =write-package-files=.  These keywords will be used for the call
  to =write-package-files= for that package.
- The =default-all=, =default-system=, and =default-path= arguments give the
  default arguments for the calls to =write-package-files=.
- If =package-extension= is non-nil (its default is =t=), then whenever a
  package spec does not give an explicit path, it should use a subdirectory of
  the default path whose name is taken from the package.  If
  =extension-downcase= is non-nil \(its default is =t=), then the package name
  is converted to lower-case for this extension.
- The =index-acc= is a hash-table used to accumulate symbol references for an
  index page, or =nil= if no index data should be saved.
- The =show-package=, =hoist-exported=, and =page-title= keyword arguments
  direct the formatting of the index page which this routine should create when
  it is the entry call to this package.
  - If =show-package= is non-nil, then the symbol's package name will be
    dsplayed on any index page.
  - If =hoist-exported= in non-nil, then the list of symbols will be divided
    according to the package which exports each symbol, or by internal symbols
    of all packages.
  - If non-nil, =page-title= should be a string to be used as the page name."
  (let ((*index-writer* nil)
        (*section-level* section-level)
        (*show-title* show-title)
        (*show-usage-header* package-headers)
        (*show-package-header* usage-headers)
        (*standard-doctype-alist* types)
        (*latex-label-suffix* latex-label-suffix))
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
                   (setf path (merge-pathnames (concatenate 'string extension
                                                            "/")
                                               path)))))
              (t (setf path (merge-pathnames subdir path))))
            (write-package-files package :all all :system system :path path
                                 :index-acc index-acc
                                 :always-disambiguate always-disambiguate))))
  (when (and *index-writer* index)
    (format t "Writing index for packages.~%")
    (write-index-org index-acc
                     (asdf:system-relative-pathname index-system default-path)
                     index :show-package show-package
                     :hoist-exported hoist-exported :title page-title))
  (values))

(defun set-index-usage (index-acc symbol usage path)
  "The =index-acc= hash table builds up path information for an index."

  ;; Pull the hash table for the SYMBOL, creating and caching one if
  ;; needed.
  (let ((symbol-hash (gethash symbol index-acc)))
    (unless symbol-hash
      (setf symbol-hash (make-hash-table :test 'eq)
            (gethash symbol index-acc) symbol-hash))

    ;; Store the path for this use of the symbol.
    (setf (gethash usage symbol-hash) path))
  (values))

(defun write-index-org (index-acc directory-path index-file-name
                        &key show-package hoist-exported title)
  "Write the accumulated indexing information to its own Org file.
- The =show-package=, =hoist-exported=, and =page-title= keyword arguments
  direct the formatting of the index page.
  - If =show-package= is non-nil, then a symbol's package name will be
    dsplayed in its list entry.
  - If =hoist-exported= in non-nil, then the list of symbols will be divided
    according to the package which exports each symbol, or by internal symbols
    of all packages.
  - If non-nil, =page-title= should be a string to be used as the page name."
  (unless (stringp index-file-name) (setf index-file-name "index"))
  (let* ((package-hash (make-hash-table :test 'eq))
         (path (merge-pathnames (concatenate 'string index-file-name ".org")
                                directory-path)))
    (iter (for (key val) in-hashtable index-acc)
          (declare (ignore val))
          (cond
            (hoist-exported
             (multiple-value-bind (echo status) (intern (symbol-name key)
                                                        (symbol-package key))
               (declare (ignore echo))
               (cond
                 ((eq status :external)
                  (push key (gethash (symbol-package key) package-hash)))
                 (t (collect key into names)))))
            (t (collect key into names)))
          (finally (with-open-file (out path :direction :output
                                        :if-exists :supersede)
                     (when title (format out "#+TITLE: ~a~%" title))
                     (iter (for (package pnames) in-hashtable package-hash)
                           (for sectioned initially nil then t)
                           (format out "* Exported from =~a=~%"
                             (package-name package))
                           (appending (write-names-list out pnames nil
                                                        index-acc)
                                      into evals)
                           (finally
                            (when sectioned (format out "* Internal names~%"))
                            (let ((last-evals
                                   (when names
                                     (write-names-list out names show-package
                                                       index-acc))))
                              (when *generate-html*
                                (uiop:run-program
                                 `("emacs" ,@evals ,@last-evals
                                           ,@(export-to-html-arguments (uiop:subpathname directory-path "index.org"))
                                           "--eval" "(save-buffers-kill-terminal)"))))))))))
  (values))

(defun export-to-html-arguments (pathname)
  (list "--eval" (format nil "(find-file ~S)" (uiop:native-namestring pathname))
        "--eval" "(org-html-export-to-html)"))

(defun write-names-list (out names show-package index-acc)
  (iter NAME-ITER
        (for name in (sort names #'string< :key #'symbol-name))
        (format out "- =~a= (" name)
        (when show-package
          (format out "package =~a=" (package-name (symbol-package name))))
        (iter (for (usage path) in-hashtable (gethash name index-acc))
              (for sep initially (if show-package ", " "") then ", ")
              (for fin initially "" then ")")
              (format out "~a[[file:~a][~a]]" sep path usage)
              (in NAME-ITER
                  (appending (export-to-html-arguments path) into evals))
              (finally (format out "~a" fin)))
        (format out ".~%")
        (finally (return-from write-names-list evals))))

;;; -----------------------------------------------------------------

#-allegro (defvar +nonstandard-doc-type+ (make-hash-table :test 'eq)
            "Hash table for non-standard documentation types.")

#+allegro (declaim (inline documentation*))
(defun documentation* (x doc-type)
  "Sometimes you may want to maintain documentation attached to
non-standard targets in the source code, and pull those strings into
the manual (for example, command options or available quoted forms).
Some Lisp implementations make it easy to use non-standard targets to
=documentation=; others essentially forbid it.  The =documentation*=
and =(setf documentation*)= functions manage the nonstandard documentation
types on platforms which do not allow it.
#+begin_example
\(documentation* X DOC-TYPE)
\(setf (documentation* X DOC-TYPE) DOC-STRING)
#+end_example
At this time, on Allegro Lisp these functions are inlined wrappers for the
standard =documentation= functions, and on other platforms it filters out the
non-standard types and stores them in the =+nonstandard-doc-type+= hash.

Note that when using =(setf documentation*)= to install documentation strings
of non-standard doc-type it is necessary to load the =org-sampler= ASDF system
as a prerequisite of the documented system.  This is in contrast to the use
case of merely assembling documentation separately from loading the system,
in which case there is no reason to =:depends-on= the Org-Sampler system.
"
  #+allegro (documentation x doc-type)
  #-allegro (case doc-type
              ((t function setf compiler-macro method-combination type
                  structure variable)
               (documentation x doc-type))
              (otherwise
               (multiple-value-bind (name-hash hash-supp)
                   (gethash doc-type +nonstandard-doc-type+)
                 (when hash-supp
                   (gethash x name-hash))))))

(defun (setf documentation*) (docstring x doc-type)
  #+allegro (setf (documentation x doc-type) docstring)
  #-allegro (case doc-type
              ((t function setf compiler-macro method-combination type
                  structure variable)
               (documentation x doc-type))
              (otherwise
               (multiple-value-bind (name-hash hash-supp)
                   (gethash doc-type +nonstandard-doc-type+)
                 (unless hash-supp
                   (setf name-hash (make-hash-table :test 'eq)
                         (gethash doc-type +nonstandard-doc-type+) name-hash))
                 (setf (gethash x name-hash) docstring)))))

;;; -----------------------------------------------------------------

(defun self-document ()
  "Applies =Org-Sampler= to itself in its own directory."
  (let ((path (asdf:system-relative-pathname :org-sampler "doc/")))
    (write-package-files :org-sampler :path path :index "index.org"
                         :show-package nil :hoist-exported t :section-level 3
                         :package-headers nil :usage-headers nil :show-title nil
                         :page-title "Org-Sampler")))

