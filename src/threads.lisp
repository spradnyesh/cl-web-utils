(in-package :web-utils)

(defmacro do-in-background (fn)
  (let ((status (gensym)))
    `(plet ((,status ,fn))
       (declare (ignore ,status)))))
