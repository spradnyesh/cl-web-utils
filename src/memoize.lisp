(in-package :web-utils)

(defvar *mem-tables* nil)

(defun memoize (fn)
  "call: (defvar m-fib (memoize 'fib))
   use:  (funcall m-fib 35)"
  (let* ((hm (make-hash-table :test 'equal))
         (mem-fn (lambda (&rest args)
                   (let ((e (gethash args hm)))
                     (if e
                         e
                         (let ((ret (apply fn args)))
                           (setf (gethash args hm) ret)
                           ret))))))
    (push (cons (concatenate 'string
                             (package-name (symbol-package fn))
                             "::"
                             (symbol-name fn))
                hm)
          *mem-tables*)
    (values mem-fn hm)))
