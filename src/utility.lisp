;;;; utility.lisp - general utility functions.

(in-package #:cl-renoise)

(defun escape-string (string)
  "Escape STRING's double quotes."
  (loop :for char :across string
        :if (char= #\" char)
          :append (list #\BACKSLASH #\QUOTATION_MARK) :into res
        :else
          :collect char :into res
        :finally (return (coerce res 'string))))
