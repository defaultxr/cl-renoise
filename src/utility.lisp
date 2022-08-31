;;;; utility.lisp - general utility functions.

(in-package #:cl-renoise)

(defun escape-string (string)
  "Escape STRING's double quotes."
  (apply #'concatenate 'string
         (map 'list (lambda (c)
                      (if (char= #\" c) "\\\"" c))
              string)))
