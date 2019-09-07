(in-package #:cl-renoise)

(defun concat (&rest strings)
  "Obligatory string concatenation utility function."
  (format nil "~{~A~}" (remove-if #'null strings)))

(defun escape-string (string)
  "Escape STRING's double quotes."
  (apply 'concat
         (map 'list (lambda (c)
                      (if (char= #\" c) "\\\"" c))
              string)))
