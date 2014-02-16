(in-package :web-utils-tests)

(def-suite :file :in :web-utils)
(in-suite :file)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; mock functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun hunchentoot::user-agent ()
  "Windows")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(test normalize-file-name
  (is (string-equal "abc.txt"
                    (web-utils::normalize-file-name "c:\\abc.txt"))))
