(in-package :web-utils-tests)

(def-suite :html :in :web-utils)
(in-suite :html)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; mocks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun translate (str)
  str)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; timed-redirect
(test timed-redirect
  (is (equal (macroexpand-1 '(timed-redirect))
             '(<:SCRIPT :TYPE "text/javascript"
               (FORMAT NIL "setTimeout('location.href=\"~a\"', 5000);" RESTAS:ROUTE)))))

;; click-here
(test click-here
  (is (equal (macroexpand-1 '(click-here "password-email"
                              (h-gen-full-url 'r-password-change-get
                               :hash hash
                               :lang (get-dimension-value "lang"))))
             '(TRANSLATE "password-email"
               (<:A :HREF
                (H-GEN-FULL-URL 'R-PASSWORD-CHANGE-GET :HASH HASH :LANG
                 (GET-DIMENSION-VALUE "lang"))
                (TRANSLATE "here"))))))

;; tooltip
(test tooltip-1
  (is (equal (macroexpand-1 '(tooltip "abc"))
             '(<:SPAN :CLASS "tooltip" (<:SUP "#")
               (<:SPAN :CLASS "classic" (TRANSLATE "abc"))))))
(test tooltip-2
  (is (equal (macroexpand-1 '(tooltip "abc" :marker "@"))
             '(<:SPAN :CLASS "tooltip" (<:SUP "@")
               (<:SPAN :CLASS "classic" (TRANSLATE "abc"))))))
(test tooltip-3
  (is (equal (macroexpand-1 '(tooltip "abc" :class "c"))
             '(<:SPAN :CLASS "tooltip" (<:SUP "#")
               (<:SPAN :CLASS "c" (TRANSLATE "abc"))))))
(test tooltip-4
  (is (equal (macroexpand-1 '(tooltip "abc" :marker "@" :class "c"))
             '(<:SPAN :CLASS "tooltip" (<:SUP "@")
               (<:SPAN :CLASS "c" (TRANSLATE "abc"))))))

;; tr-td-helper
(test tr-td-helper
  (is (equal (macroexpand-1 '(tr-td-helper (<:input :type typeof
                                            :name for
                                            :value value
                                            (when disabled :disabled))))
             '(TR-TD-HELPER
               (<:INPUT :TYPE TYPEOF :NAME FOR :VALUE VALUE (WHEN DISABLED :DISABLED))))))

;; label-input
(test label-input
  (is (equal (macroexpand-1 '(label-input "username" "text"))
             '(<:DIV
               (<:P
                (<:LABEL :CLASS "label" :FOR "username"
                 (IF NIL
                     (FMTNIL (TRANSLATE "username") (<:SPAN :CLASS "mandatory" "*"))
                     (FMTNIL (TRANSLATE "username"))))
                (WHEN NIL (TOOLTIP NIL)))
               (<:INPUT :CLASS "input" :TYPE "text" :NAME "username" :ID "username")))))

;; submit-success
(test submit-success-t
  (is (string-equal (macroexpand-1 (submit-success t (h-genurl 'r-account-get)))
                    "{\"status\":\"success\",\"data\":\"\\/\"}")))
;; TODO
#|(test submit-success-nil
  (let ((hunchentoot:*request* t))
    (is (string-equal (macroexpand-1 (submit-success nil (h-genurl 'r-account-get)))
                      "{\"status\":\"success\",\"data\":\"\\/\"}"))))|#

;; submit-error
(test submit-error
  (is (equal (macroexpand-1 '(submit-error ajax
                              err0r
                              (if id
                                  (h-genurl 'r-article-edit-get :id (write-to-string id))
                                  (h-genurl 'r-article-new-get))))
             '(IF AJAX
               (JSON:ENCODE-JSON-TO-STRING
                `((:STATUS . "error") (:MESSAGE ,@(TRANSLATE "submit-error"))
                  (:ERRORS ,@(REVERSE ERR0R))))
               (RESTAS:REDIRECT
                (IF ID
                    (H-GENURL 'R-ARTICLE-EDIT-GET :ID (WRITE-TO-STRING ID))
                    (H-GENURL 'R-ARTICLE-NEW-GET)))))))

;; cannot-be-empty
(test cannot-be-empty-1
  (is (equal (macroexpand-1 '(cannot-be-empty title "title" err0r))
             '(IF (IS-NULL-OR-EMPTY TITLE)
               (PUSH (TRANSLATE (FORMAT NIL "~a-cannot-be-empty" "title")) ERR0R)))))

(test cannot-be-empty-2
  (is (equal (macroexpand-1 '(cannot-be-empty zipcode "zipcode" err0r
                              (print 1)))
             '(IF (IS-NULL-OR-EMPTY ZIPCODE)
               (PUSH (TRANSLATE (FORMAT NIL "~a-cannot-be-empty" "zipcode")) ERR0R)
               (PRINT 1)))))

;; tr-td-input
(test tr-td-input-1
  (is (string-equal (tr-td-input "title")
                    "<tr id=\"\" class=\"\"><td class=\"label\"><label for=\"title\">title</label></td><td><input type=\"text\" name=\"title\" value=\"\" nil=\"\"/></td></tr>")))

(test tr-td-input-2
  (is (string-equal (tr-td-input "title" :typeof "file")
                    "<tr id=\"\" class=\"\"><td class=\"label\"><label for=\"title\">title</label></td><td><input type=\"file\" name=\"title\" value=\"\" nil=\"\"/></td></tr>")))

(test tr-td-input-3
  (is (string-equal (tr-td-input "password" :typeof "password" :mandatory t)
                    "<tr id=\"\" class=\"\"><td class=\"label\"><label for=\"password\">password<span class=\"mandatory\">*</span></label></td><td><input type=\"password\" name=\"password\" value=\"\" nil=\"\"/></td></tr>")))

(test tr-td-input-4
  (is (string-equal (tr-td-input "response" :class "hidden" :id t :value "")
                    "<tr id=\"response\" class=\"hidden\"><td class=\"label\"><label for=\"response\">response</label></td><td><input type=\"text\" name=\"response\" value=\"\" nil=\"\"/></td></tr>")))

;; tr-td-text
(test tr-td-text
  (is (string-equal (tr-td-text "body"
                                :class "ckeditor"
                                :value "abc"
                                :mandatory t)
                    "<tr id=\"\" class=\"ckeditor\"><td class=\"label\"><label for=\"body\">body<span class=\"mandatory\">*</span></label></td><td><textarea cols=\"40\" name=\"body\" rows=\"7\">abc</textarea></td></tr>")))

;; tr-td-submit
(test tr-td-submit
  (is (string-equal (tr-td-submit)
                    "<tr><td></td><td><input type=\"submit\" name=\"submit\" class=\"submit\" value=\"submit\"/></td></tr>")))

;; validate-email
(test validate-email-t-1
  (is-true (validate-email "abc@def.com")))

(test validate-email-t-2
  (is-true (validate-email "a@d.abc")))

(test validate-email-nil-1
  (is-false (validate-email "abc")))

(test validate-email-nil-2
  (is-false (validate-email "a@d.c")))
