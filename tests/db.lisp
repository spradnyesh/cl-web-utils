(in-package :web-utils-tests)

(def-suite :db :in :web-utils)
(in-suite :db)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; mocks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun web-utils::get-config (key dim-str)
  (declare (ignore dim-str))
  key)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; get-storage
(test get-storage
  (is (equal (macroexpand-1 '(web-utils::get-storage :articles "master"))
             '(COND
               ((EQUAL (GET-CONFIG "db.type" "master") "prevalence")
                (CL-PREVALENCE:GET-ROOT-OBJECT WEB-UTILS::SYSTEM :ARTICLES))))))

;; init-db-system
; TODO: gensym
#|(test init-db-system
  (is (equal (macroexpand-1 '(init-db-system "article" :articles dim-str))
             '(LET ((#:G1360 (GET-CONFIG "db.type" DIM-STR)))
               (COND
                 ((EQUAL #:G1360 "prevalence")
                  (PROGN
                    (DEFUN MAKE-ARTICLES-ROOT (WEB-UTILS::SYSTEM)
                      (SETF (CL-PREVALENCE:GET-ROOT-OBJECT WEB-UTILS::SYSTEM :ARTICLES)
                            (MAKE-INSTANCE 'ARTICLE-STORAGE)))
                    (DEFUN GET-ALL-ARTICLES ()
                      (ARTICLES (CL-PREVALENCE:GET-ROOT-OBJECT (GET-DB-HANDLE) :ARTICLES)))
                    (DEFUN INCF-ARTICLE-LAST-ID (WEB-UTILS::SYSTEM)
                      (LET ((WEB-UTILS::STORAGE (WEB-UTILS::GET-STORAGE :ARTICLES DIM-STR)))
                        (INCF (LAST-ID WEB-UTILS::STORAGE))))
                    (DEFUN INSERT-ARTICLE (WEB-UTILS::SYSTEM WEB-UTILS::OBJECT)
                      (LET ((WEB-UTILS::STORAGE (WEB-UTILS::GET-STORAGE :ARTICLES DIM-STR)))
                        (PUSH WEB-UTILS::OBJECT (ARTICLES WEB-UTILS::STORAGE))))
                    (DEFUN UPDATE-ARTICLE (WEB-UTILS::SYSTEM WEB-UTILS::OBJECT)
                      (LET* ((WEB-UTILS::STORAGE (WEB-UTILS::GET-STORAGE :ARTICLES DIM-STR))
                             (LIST (ARTICLES WEB-UTILS::STORAGE)))
                        (SETF (NTH
                               (POSITION (CL-PREVALENCE:ID WEB-UTILS::OBJECT) LIST :KEY
                                         #'CL-PREVALENCE:ID)
                               LIST)
                              WEB-UTILS::OBJECT)))
                    (DEFUN GET-ARTICLE-BY-ID (CL-PREVALENCE:ID)
                      (FIND CL-PREVALENCE:ID (GET-ALL-ARTICLES) :KEY #'CL-PREVALENCE:ID)))))))))|#

;; get-object-by
(test get-object-by
  (is (equal (macroexpand-1 '(get-object-by #'(lambda (comment)
                                                (= (article-id comment)
                                                   article-id))
                              (get-all-comments)))
             '(CONDITIONALLY-ACCUMULATE
               #'(LAMBDA (COMMENT) (= (ARTICLE-ID COMMENT) ARTICLE-ID)) (GET-ALL-COMMENTS)))))

;; db-execute
(test db-execute-nil
  (is (equal (macroexpand-1 '(db-execute 'a))
             '(IF NIL
               (CL-PREVALENCE:EXECUTE (GET-DB-HANDLE)
                (CL-PREVALENCE:MAKE-TRANSACTION 'A NIL))
               (CL-PREVALENCE:EXECUTE (GET-DB-HANDLE) (CL-PREVALENCE:MAKE-TRANSACTION 'A))))))

(test db-execute-t
  (is (equal (macroexpand-1 '(db-execute 'a 1))
             '(IF 1
               (CL-PREVALENCE:EXECUTE (GET-DB-HANDLE)
                (CL-PREVALENCE:MAKE-TRANSACTION 'A 1))
               (CL-PREVALENCE:EXECUTE (GET-DB-HANDLE) (CL-PREVALENCE:MAKE-TRANSACTION 'A))))))

;; get-db-handle
(test get-db-handle
  (with-fixture resources ()
      (is (string-equal (get-db-handle)
                        "db-value"))))
