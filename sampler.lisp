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

(defun write-symbol-files (symbol directory-path &key always-disambiguate)
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
      (write-fn-file symbol (merge-pathnames
                             (concatenate 'string
                               use-name (if disambiguate "__fn" "") ".org")
                             directory-path)))
    (when is-type
      (write-type-file symbol
                       (merge-pathnames
                        (concatenate 'string
                          use-name (if disambiguate "__type" "") ".org")
                        directory-path)))
    (when is-variable
      (write-variable-file symbol
                       (merge-pathnames
                        (concatenate 'string
                          use-name (if disambiguate "__variable" "") ".org")
                        directory-path)))))

(defun write-package-files (package &key all system path)
  (unless path (setf path #p"./"))
  (when system (setf path (asdf:system-relative-pathname system path)))

  (ensure-directories-exist path)
  (cond (all (do-symbols (sym package)
               (when (eq (symbol-package sym) (find-package package))
                 (write-symbol-files sym path))))
        (t (do-external-symbols (sym package)
             (write-symbol-files sym path)))))
