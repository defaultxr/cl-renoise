;;;; renoise.asd

(asdf:defsystem #:renoise
  :description "Lisp interface to Renoise"
  :author "modula t. <defaultxr at gmail>"
  :license "MIT"
  :version "0.1"
  :serial t
  :depends-on (#:usocket
               #:osc)
  :components ((:file "package")
               (:file "renoise")))
