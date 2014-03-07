(in-package :web-utils-tests)

(def-suite :dimensions :in :web-utils)
(in-suite :dimensions)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; fixtures and mocks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def-fixture resources ()
  (setf *default-dimensions* "master")

  (setf *resources* (make-hash-table :test 'equal))
  (setf (gethash "master" *resources*) (make-hash-table :test 'equal))

  (setf (gethash "key" (gethash "master" *resources*)) "value")
  (setf (gethash "db" (gethash "master" *resources*)) "db-value")

  (&body))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; get-resource
(test get-resource
  (with-fixture resources ()
    (is (string-equal (web-utils::get-resource "key" "master")
                      "value"))))

;; set-resource
(test set-resource
  (with-fixture resources ()
    (web-utils::set-resource "name" "value" "master")
    (is (string-equal (web-utils::get-resource "key" "master")
                      "value"))))

;; show-resources
(test show-resources
  (let ((fstr (make-array '(0) :element-type 'base-char
                          :fill-pointer 0 :adjustable t)))
    (with-output-to-string (s fstr)
      (with-fixture resources ()
        (show-resources s)
        (is (string-equal fstr
                          "***** master: *****
key: value
db: db-value
"))))))

;; find-dimension-value
(test find-dimension-value
  (with-fixture resources ()
    (is (string-equal (web-utils::find-dimension-value "key")
                      "value"))))
