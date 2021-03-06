;;; -*- Mode: common-lisp; package: org-sampler-asd; -*-

;;; ORG-SAMPLER - Use Lisp docstrings and reflection to make org-mode text for
;;; inclusion into a larger document.
;;;
;;; File: org-sampler.asd
;;;

(defsystem "org-sampler"
    :description "Extract docstrings as Emacs org-mode files"
    :version "0.2.1"
    :author "John Maraist <lisper@maraist.org>"
    :license "LLGPL 3.latest"
    :depends-on ("iterate")
    :components ((:file "package")      ; Lisp infrastructure.
                 (:file "sampler" :depends-on ("package"))))
