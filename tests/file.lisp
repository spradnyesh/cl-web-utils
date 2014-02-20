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
;; normalize-file-name
(test normalize-file-name
  (is (string-equal "abc.txt"
                    (web-utils::normalize-file-name "c:\\abc.txt"))))

;; get-directory-path-string
(test get-directory-path-string
  (is (string-equal (get-directory-path-string #p"/home/pradyus/pers/../pers/vc/a.html")
                    "/home/pradyus/pers/../pers/vc/")))

;; get-new-path
;; TODO
#|(test get-new-path
  (is (equal (web-utils::get-new-path #p"/home/pradyus/pers/vc/a.html")
             #P"/home/pradyus/pers/vc/a1.html")))|#
