(in-package :web-utils)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; helper macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro lang-a (lang selected lang-name href)
  (with-gensyms (class)
    `(let ((,class (concatenate 'string
                                "small"
                                (when (or (string= ,lang "hi-IN")
                                          (string= ,lang "mr-IN"))
                                  " dvngr"))))
       (<:li (<:a :class (if ,selected
                             (concatenate 'string ,class " selected")
                             ,class)
                  :href ,href
                  ,lang-name)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; helper functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun logo-langs (en-href mr-href hi-href)
  (declare (ignore hi-href))
  (<:ul :class "langs"
        (let ((lang (get-dimension-value "lang")))
          (cond ((string-equal lang "en-IN")
                 (fmtnil (lang-a "en-IN" t "English" en-href)
                         #- (and)
                         (lang-a "hi-IN" nil "हिन्दी" hi-href)
                         (lang-a "mr-IN" nil "मराठी" mr-href)))
                #- (and)
                ((string-equal lang "hi-IN")
                 (fmtnil (lang-a "en-IN" nil "English" en-href)
                         (lang-a "hi-IN" t "हिन्दी" hi-href)
                         (lang-a "mr-IN" nil "मराठी" mr-href)))
                ((string-equal lang "mr-IN")
                 (fmtnil (lang-a "en-IN" nil "English" en-href)
                         #- (and)
                         (lang-a "hi-IN" nil "हिन्दी" hi-href)
                         (lang-a "mr-IN" t "मराठी" mr-href)))
                (t (redirect (h-genurl 'r-login-get :lang "en-IN")))))))

(defun load-language (file-path)
  (let ((ht (make-hash-table :test 'equal))
        (rslt nil))
    (with-open-file (stream file-path)
      (do ((line (read-line stream nil)
                 (read-line stream nil)))
          ((null line))
        (setf rslt (split-sequence "=" line :test #'string=))
        (setf (gethash (first rslt) ht)
              (join-string-list-with-delim "=" (rest rslt)))))
    ht))

(defun get-translation (key)
  (let ((out-hash (gethash (get-dimension-value "lang") *translation-table*)))
    (if out-hash
        (let ((rslt (gethash key out-hash)))
          (if rslt
              rslt
              (gethash key (gethash (get-config "site.lang") ; everything falls back to site.lang
                              *translation-table*))))
        (gethash key (gethash (get-config "site.lang") ; everything falls back to site.lang
                              *translation-table*)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; APIs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun load-all-languages (&optional (locale-path (get-config "path.locale")))
  (setf *translation-table* (make-hash-table :test 'equal))
  (dolist (f (directory (concatenate 'string
                                     (get-directory-path-string locale-path) "??-??.lisp")))
    (setf (gethash (pathname-name f) *translation-table*) (load-language f))))

(defun show-translation-tree ()
  (maphash #'(lambda (k-out v-out)
               (format t "########## ~a~%" k-out)
               (maphash #'(lambda (k-in v-in)
                            (format t "~a: ~a~%" k-in v-in))
                        v-out)
               (format t "~%"))
           *translation-table*))

;; if #params are less than #~a in translated format-string
;; then die silently and return the key instead of an error
(defun translate (key &rest params)
  (handler-case (apply #'format
                       nil
                       (let ((trans (get-translation key)))
                         (if trans
                             trans
                             (string-capitalize (regex-replace-all "-" key " "))))
                       params)
           (sb-format:format-error ()
             (concatenate 'string "[[[" key "]]]"))))
