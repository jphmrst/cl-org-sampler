;;; -*- Mode: common-lisp; package: org-sampler-asd; -*-

;;; ORG-SAMPLER - Use Lisp docstrings and reflection to make org-mode text for
;;; inclusion into a larger document.
;;;
;;; File: org-sampler.asd
;;;

(defpackage :org-sampler-asd (:use :common-lisp :asdf))
(in-package :org-sampler-asd)

(defsystem :org-sampler
    :version "0.2.0"
    :depends-on (:iterate)
    :components ((:file "package")      ; Lisp infrastructure.
                 (:file "sampler" :depends-on ("package"))))

