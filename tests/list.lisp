(in-package :web-utils-tests)

(test conditionally-accumulate-t
  (is (equal '(2 4)
             (conditionally-accumulate #'evenp '(1 2 3 4 5)))))
(test conditionally-accumulate-f
  (is (not (equal '(1 3)
                  (conditionally-accumulate #'evenp '(1 2 3 4 5))))))

(test replace-all-t
  (is (equal '(1 0 3 4 5)
             (replace-all '(1 2 3 4 5) 2 0))))
(test replace-all-f
  (is (not (equal '(1 0 3 4 5)
                  (replace-all '(1 2 3 4 5) 3 0)))))

(test insert-at-t
  (is (equal '(1 2 3 4 5)
             (insert-at '(1 3 4 5) 2 1))))
(test insert-at-f
  (is (not (equal '(1 2 3 4 5)
                  (insert-at '(1 3 4 5) 2 2)))))

(test splice-1
  (is (equal '(1 2 3 4 5)
             (splice '(1 2 3 4 5)))))
(test splice-2
  (is (equal '(2 3 4 5)
             (splice '(1 2 3 4 5) :from 1))))
(test splice-3
  (is (equal '(2 3 4)
             (splice '(1 2 3 4 5) :from 1 :to 3))))
(test splice-4
  (multiple-value-bind (a b)
      (splice '(1 2 3 4 5) :from 1 :to 5)
    (is (and (null a)
             (equal '(1 2 3 4 5)
                    b)))))
(test splice-5
  (multiple-value-bind (a b)
      (splice '(1 2 3 4 5) :from 5 :to 3)
    (is (and (null a)
             (equal '(1 2 3 4 5)
                    b)))))
