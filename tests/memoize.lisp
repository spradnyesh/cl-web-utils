(in-package :web-utils-tests)

(defvar *mem-map* (init-memoize))

(defun fib (n)
  (if (< n 3)
      1
      (+ (fib (1- n)) (fib (- n 2)))))

(test clr-memoize
  (with-mem-map (*mem-map*)
    (clr-memoize)
    (is (zerop (hash-table-count *mem-map*)))))

(test memoize-1
  (with-mem-map (*mem-map*)
    (clr-memoize)
    (multiple-value-bind (fn status)
        (memoize 'fib)
      (is (and status
               ;; this is actually a closure; i dunno why it shows as function inside the test
               (equal 'function (type-of fn)))))))

(test memoize-2
  (with-mem-map (*mem-map*)
    (clr-memoize)
    (memoize 'fib)
    (multiple-value-bind (fn status)
        (memoize 'fib)
      (is (and (null status)
               (equal 'cons (type-of fn))
               (equal 'function (type-of (first fn))))))))

(test memoize-3
  (with-mem-map (*mem-map*)
      (clr-memoize)
    (memoize 'fib)
    (multiple-value-bind (fn status)
        (memoize 'fib :re-memoize t)
      (is (and status
               (equal 'function (type-of fn)))))))

(test un-memoize-1
  (with-mem-map (*mem-map*)
      (clr-memoize)
    (is (null (un-memoize 'fib)))))

(test un-memoize-2
  (with-mem-map (*mem-map*)
      (clr-memoize)
    (memoize 'fib)
    (is (equal 'symbol (type-of (un-memoize 'fib))))))

(test memoized-defun
  (with-mem-map (*mem-map*)
    (is (equal (macroexpand-1 '(memoized-defun fib (n)
                                (if (< n 3)
                                    1
                                    (+ (fib (1- n)) (fib (- n 2))))))
               '(PROGN
                 (DEFUN FIB (N)
                   (IF (< N 3)
                       1
                       (+ (FIB (1- N)) (FIB (- N 2)))))
                 (MEMOIZE 'FIB))))))
