(in-package :hawksbill.utils)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; helper functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

(defun get-translation (key &optional (lang *lang*))
  (gethash key (gethash lang *translation-table*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; init *lang* for every request (as shown in http://restas.lisper.ru/en/manual/decorators.html)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass lang-route (routes:proxy-route) ())

(defmethod process-route :before ((route lang-route) bindings)
  (let ((host (host))
        (index nil)
        (lang nil))
    (cond
      ;; localhost/dev
      ((search "localhost" host)
       (if (setf lang (get-parameter "lang"))
           (setf *lang* lang)
           (setf *lang* (get-config "site.lang"))))
      ;; TODO: int/qa
      ;; production
      ((setf index (search (get-config "site.url") host))
       (setf host (subseq host 0 index))
       (cond ((equal host "mr") (setf *lang* "mr-IN"))
             ((equal host "hi") (setf *lang* "hi-IN"))
             (t (setf *lang* "en-IN")))))))

(defun @init-lang (route)
  (make-instance 'lang-route :target route))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; APIs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun load-all-languages (&optional (locale-path (get-config "path.locale")))
  (setf *translation-table* (make-hash-table :test 'equal))
  (dolist (f (directory (concatenate 'string
                                     (get-parent-directory-path-string locale-path) "??-??.lisp")))
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
;; then die silently and return nil instead of an error
(defun translate (key &rest params)
  (handler-case (apply #'format nil (get-translation key) params)
    (sb-format:format-error () nil)))
