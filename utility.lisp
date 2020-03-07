(in-package #:cl-renoise)

(defun escape-string (string)
  "Escape STRING's double quotes."
  (apply 'concat
         (map 'list (lambda (c)
                      (if (char= #\" c) "\\\"" c))
              string)))
