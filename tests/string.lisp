(in-package :web-utils-tests)

(def-suite :string :in :web-utils)
(in-suite :string)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; mock functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; fmtnil
(test fmtnil-1
  (is (string-equal "ab"
                    (fmtnil "a" "b"))))
(test fmtnil-2
  (is (string-equal "ab"
                    (fmtnil "a" "" "b"))))

;; string-to-utf-8
(test string-to-utf-8-1
  (is (equal '(HANDLER-CASE
               (TRIVIAL-UTF-8:UTF-8-BYTES-TO-STRING
                (FLEXI-STREAMS:STRING-TO-OCTETS (SLUG TAG) :EXTERNAL-FORMAT :UTF-8))
               (FLEXI-STREAMS:EXTERNAL-FORMAT-ENCODING-ERROR NIL NIL))
             (macroexpand-1 '(string-to-utf-8 (slug tag) :utf-8)))))
(test string-to-utf-8-1
  (is (equal '(HANDLER-CASE
               (TRIVIAL-UTF-8:UTF-8-BYTES-TO-STRING
                (FLEXI-STREAMS:STRING-TO-OCTETS CAT-SLUG :EXTERNAL-FORMAT :LATIN1))
               (FLEXI-STREAMS:EXTERNAL-FORMAT-ENCODING-ERROR NIL NIL))
             (macroexpand-1 '(string-to-utf-8 cat-slug :latin1)))))

;; join-loop
#|(test join-loop
  (is (equal '(LOOP PARENSCRIPT:FOR I PARENSCRIPT:IN (RANGE 24)
                 WEB-UTILS::COLLECTING (FMTNIL
                                        (<:OPTION :VALUE I I)) WEB-UTILS::INTO #:a
                 WEB-UTILS::FINALLY (RETURN (APPLY #'CONCATENATE 'STRING #:)))
             (macroexpand-1 '(join-loop i (range 24)
                              (fmtnil (<:option :value i i)))))))|#

;; string-pad
(test string-pad-1
  (is (string-equal (string-pad "abc" #\c 5)
                    "ccabc")))
(test string-pad-2
  (is (string-equal (string-pad "abc" #\d 5 :r)
                    "abcdd")))
(test string-pad-3
  (is (string-equal (string-pad "abcdef" #\d 5 :r)
                    "abcdef")))

;; join-string-list-with-delim
(test join-string-list-with-delim-1
  (is (string-equal (join-string-list-with-delim ";" '("abc" "def" "ghi"))
                    "abc;def;ghi")))
(test join-string-list-with-delim-2
  (is (string-equal (join-string-list-with-delim ";" '(("abc" 1) ("def" 2) ("ghi" 3)) :key #'first)
                    "abc;def;ghi")))

;; split-string-by-delim
(test split-string-by-delim-1
  (is (equal (split-string-by-delim "a b c" " ")
          '("a" "b" "c"))))
(test split-string-by-delim-2
  (is (equal (split-string-by-delim "a  b c" " ")
          '("a" "b" "c"))))

;; nil-or-empty
(test nil-or-empty-1
  (is-true (nil-or-empty "")))
(test nil-or-empty-2
  (is-true (nil-or-empty nil)))
(test nil-or-empty-3
  (is-false (nil-or-empty "a")))
(test nil-or-empty-3
  (is-false (nil-or-empty 3)))

;; slugify
(test slugify-1
  (is (equal (slugify "http://stackoverflow.com/questions/211717/common-lisp-programmatic-keyword")
             "httpstackoverflowcomquestions211717common-lisp-programmatic-keyword")))
(test slugify-2
  (is (equal (slugify "edited Mar 27 '13 at 17:08")
             "edited-mar-27-13-at-1708")))
