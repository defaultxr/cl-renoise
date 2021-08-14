;;;; renoise.asd

(asdf:defsystem #:cl-renoise
  :name "cl-renoise"
  :description "Lisp interface to Renoise"
  :author "modula t."
  :license "MIT"
  :version "0.1"
  :homepage "https://github.com/defaultxr/cl-renoise"
  :bug-tracker "https://github.com/defaultxr/cl-renoise/issues"
  :mailto "defaultxr at gmail dot com"
  :source-control (:git "git@github.com:defaultxr/cl-renoise.git")
  :depends-on (#:alexandria
               #:mutility
               #:usocket
               #:osc)
  :serial t
  :components ((:file "package")
               (:file "utility")
               (:file "cl-renoise")))

(asdf:defsystem #:cl-renoise/objects
  :description "Lisp interface to Renoise with CLOS objects"
  :author "modula t."
  :license "MIT"
  :version "0.1"
  :depends-on (#:cl-renoise
               #:cl-flow)
  :serial t
  :components ((:file "objects")))
