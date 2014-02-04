(in-package :web-utils-tests)

;; conditionally-accumulate
(test conditionally-accumulate-t
  (is (equal '(2 4)
             (conditionally-accumulate #'evenp '(1 2 3 4 5)))))
(test conditionally-accumulate-f
  (is (not (equal '(1 3)
                  (conditionally-accumulate #'evenp '(1 2 3 4 5))))))

;; replace-all
(test replace-all-t
  (is (equal '(1 0 3 4 5)
             (replace-all '(1 2 3 4 5) 2 0))))
(test replace-all-f
  (is (not (equal '(1 0 3 4 5)
                  (replace-all '(1 2 3 4 5) 3 0)))))

;; insert-at
(test insert-at-t
  (is (equal '(1 2 3 4 5)
             (insert-at '(1 3 4 5) 2 1))))
(test insert-at-f
  (is (not (equal '(1 2 3 4 5)
                  (insert-at '(1 3 4 5) 2 2)))))

;; splice
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

;; group-list
(test group-list-t
  (multiple-value-bind (a b)
      (group-list #'first '((1 2) (3 1) (1 3) (3 2) (2 1) (1 4) (2 3)))
    (is (and (equal a '(((2 3) (2 1)) ((3 2) (3 1)) ((1 4) (1 3) (1 2))))
             (eq (hash-table-count b) 3)
             (equal (gethash 1 b) '((1 4) (1 3) (1 2)))
             (equal (gethash 2 b) '((2 3) (2 1)))
             (equal (gethash 3 b) '((3 2) (3 1)))))))

;; append-nil
(test append-nil
  (is (equal '((1.1 1.2 nil) (2.1 2.2 nil))
             (web-utils::append-nil '((1.1 1.2) (2.1 2.2))))))

;; range
(test range-1
  (is (equal '(0 1 2 3)
             (range 4))))
(test range-2
  (is (equal '(1 2 3)
             (range 1 4))))
(test range-3
  (is (equal '(1 1.5 2.0 2.5 3.0 3.5)
             (range 1 4 0.5))))

;; permutations and combinations
(test combinations
  (is (equal '((3) (3 1) (3 2 1) (3 2) (2) (2 1) (1))
             (web-utils::combinations-i '(1 2 3)))))

(test permutations
  (is (equal '((1 2 3) (1 3 2) (2 1 3) (2 3 1) (3 1 2) (3 2 1))
             (web-utils::permutations '(1 2 3)))))

(test permutations-i
  (is (equal '((1) (2 1) (1 2) (2) (3 2) (2 3) (3 2 1) (3 1 2) (2 3 1) (2 1 3) (1 3 2)
               (1 2 3) (3 1) (1 3) (3))
             (web-utils::permutations-i '(1 2 3)))))

(test cross-product-i
  (is (equal '((2.1 3.3) (2.1 3.2) (2.1 3.1) (2.2 3.3) (2.2 3.2) (2.2 3.1) (3.3) (3.2) (3.1)
 (1.2 2.1 3.3) (1.2 2.1 3.2) (1.2 2.1 3.1) (1.2 2.2 3.3) (1.2 2.2 3.2)
 (1.2 2.2 3.1) (1.2 3.3) (1.2 3.2) (1.2 3.1) (1.1 2.1 3.3) (1.1 2.1 3.2)
 (1.1 2.1 3.1) (1.1 2.2 3.3) (1.1 2.2 3.2) (1.1 2.2 3.1) (1.1 3.3) (1.1 3.2)
 (1.1 3.1) (1.1) (1.1 2.2) (1.1 2.1) (1.2) (1.2 2.2) (1.2 2.1) (2.2) (2.1))
             (web-utils::cross-product-i '((1.1 1.2) (2.1 2.2) (3.1 3.2 3.3))))))

;; hashmap
(test print-map
  (let ((fstr (make-array '(0) :element-type 'base-char
                          :fill-pointer 0 :adjustable t))
        (hm (make-hash-table)))
    (with-output-to-string (s fstr)
      (setf (gethash "one" hm) 1)
      (print-map hm s)
      (is (string-equal "one: 1
"
                        fstr)))))

;; set
(test make-set
  (is (equal '(3 2 1)
             (make-set '(1 2 3 1 2)))))
