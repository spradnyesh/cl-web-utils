(in-package :web-utils)

(defmacro memoized-defun (fn (&rest args) &body body)
  `(progn (defun ,fn (,@args)
            ,@body)
          (memoize ',fn)))

(defun init-memoize ()
  (make-hash-table :test #'equal))

(defmacro with-mem-map ((mem-map) &body body)
  `(let ((*mem-map* ,mem-map))
     ,@body))

(defun memoize (fn &key (re-memoize nil))
  "memoize a function (fn), if not already memoized, or if re-memoize is t
if re-memoize is nil and fn is already memoized then it will not memoize it again, but will return the already memoized version
the 2nd return value tells whether fn was memoized in this call

NOTE: the original function is replaced with a closure; this has the following side-effects:
1. slime does not show signature in status-bar
2. Meta+. does not show original fn definition, but instead comes to this function
To avoid these side-effects (while debugging), un-memoize the fn

NOTE: compile/load function before re-memoizing it, because otherwise orig-fn will contain the currently memoized closure instead of the (updated) function

NOTE: a beneficial side-effect of below implementation is that for self-recursive functions, the output of recursive calls is automatically memoized"
  (let* ((key (concatenate 'string
                           (package-name (symbol-package fn))
                           "::"
                           (symbol-name fn)))
         (mem-fn (gethash key *mem-map*)))
    (if (or re-memoize
            (null mem-fn))
        (let ((orig-fn (symbol-function fn))) ; http://stackoverflow.com/a/19375845
          (let* ((hm (make-hash-table :test 'equal))
                 (mem-fn (lambda (&rest args)
                           (let ((e (gethash args hm)))
                             (if e
                                 e
                                 (let ((ret (apply orig-fn args)))
                                   (setf (gethash args hm) ret)
                                   ret))))))
            (setf (gethash key
                           *mem-map*)
                  (list orig-fn hm mem-fn))
            (setf (symbol-function fn) mem-fn)
            (values mem-fn t)))
        (values mem-fn nil))))

(defun un-memoize (fn)
  (let* ((key (concatenate 'string
                        (package-name (symbol-package fn))
                        "::"
                        (symbol-name fn)))
         (value (gethash key *mem-map*)))
    (when value
      (setf (symbol-function fn) (first value))
      (remhash key *mem-map*)
      fn)))

(defun clr-memoize ()
  (clrhash *mem-map*))
