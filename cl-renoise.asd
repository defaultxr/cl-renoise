;;;; renoise.asd

(asdf:defsystem #:cl-renoise
  :description "Lisp interface to Renoise"
  :author "modula t. <defaultxr at gmail>"
  :license "MIT"
  :version "0.1"
  :serial t
  :depends-on (#:usocket
               #:osc)
  :components ((:file "package")
               (:file "utility")
               (:file "cl-renoise")))

(asdf:defsystem #:cl-renoise/objects
  :description "Lisp interface to Renoise with CLOS objects"
  :author "modula t. <defaultxr at gmail>"
  :license "MIT"
  :version "0.1"
  :serial t
  :depends-on (#:mutility
               #:cl-renoise
               #:cl-flow)
  :components ((:file "objects")))
