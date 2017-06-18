;;;; cl-base16.asd

(asdf:defsystem #:cl-base16
  :description "Common Lisp implementation of base16"
  :author "Thomas Atkinson <thomas@pinegrove.io>"
  :license "GPLv2"
  :serial t
  :depends-on (:uiop
	       :cl-yaml
	       :cl-mustache
	       :cl-slug
	       :trivial-shell)
  :components ((:file "package")
               (:file "cl-base16")
	       (:file "builder")
	       (:file "cl-yaml-parser-fix")))
