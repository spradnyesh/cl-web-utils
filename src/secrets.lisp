(in-package :web-utils)

(defmacro populate-config-from-secret (key secrets)
  (let ((value (gensym)))
    `(when (nil-or-empty (get-config ,key))
       (if (assoc ,key ,secrets :test #'equal)
           (add-config ,key
                       (cdr (assoc ,key ,secrets :test #'equal))
                       "master")
           (progn (format t "please enter ~a key: " ,key)
                  (setf ,value (read-line))
                  (push (cons ,key ,value) ,secrets)
                  (add-config ,key ,value "master"))))))
