(in-package :web-utils-tests)

(def-suite :html :in :web-utils)
(in-suite :html)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; mocks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
(test submit-success-nil
  (let ((hunchentoot:*request* t))
    (is (string-equal (macroexpand-1 (submit-success nil (h-genurl 'r-account-get)))
                      "{\"status\":\"success\",\"data\":\"\\/\"}"))))
