(in-package :web-utils)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; session handling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro with-login (url &body body)
  `(if *session*
       (progn ,@body)
       (redirect ,url)))

(defmacro is-logged-in? ()
  `*session*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; circumvent restas removing decorators from module definition level
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro def-route (name (&rest template) &body body)
  `(define-route ,name (,@template)
     (:decorators #'web-utils:init-dimensions)
     ,@body))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; start/stop/restart
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro intern-system (form)
  `(intern (string-upcase (format nil ,form `,system))))
(defmacro start/stop/restart-system (system)
  `(progn
     (ensure-directories-exist ,(format nil "/tmp/hunchentoot/golbin/~a/" system))
     (defvar ,(intern-system "*~a-status*") nil)
     (defclass ,(intern-system "~a-acceptor") (hawksbill-acceptor)
       ()
       (:default-initargs
        :access-log-destination ,(format nil "/tmp/hunchentoot/golbin/~a/access.log" system)
         :message-log-destination ,(format nil "/tmp/hunchentoot/golbin/~a/error.log" system)))
     (defgeneric ,(intern-system "~a-start-real") (,(intern-system "~a-acceptor"))
       (:documentation "need to make this a method so that i can have a :after"))
     (defmethod ,(intern-system "~a-start-real") ((,(intern-system "~a-instance") ,(intern-system "~a-acceptor")))
       (declare (ignore ,(intern-system "~a-instance")))
       (unless *system-status*
         (setf *system-status* t)
         (init-config)
         (set-default-dimensions nil)
         (hu-init)
         (obfuscate-js)
         ;; init-model & db-connect only for the longest dim-str
         ;; ensure that the "db.path" config is present in longest dim-str
         (dolist (dim (first (group-list #'length *dimensions-combos*)))
           ;; the below 'sort' ensures that the dim-str is lexically sorted based on the dimension
           ;; this reduces permutations-i -> combinations-i
           (let ((dim-str (dim-to-dim-str dim)))
             (setf (gethash dim-str *resources*) (make-hash-table :test 'equal))
             (model-init dim-str)
             (db-connect dim-str))))
       (load-all-languages)
       (start (get-config ,(format nil "~a.restas.package" `,system))
              :port (get-config ,(format nil "~a.restas.port" `,system))
              :acceptor-class ',(intern-system "~a-acceptor")))
     (defun ,(intern-system "~a-start") ()
       (unless ,(intern-system "*~a-status*")
         (setf ,(intern-system "*~a-status*") t)
         (funcall #',(intern-system "~a-start-real")
                  (make-instance ',(intern-system "~a-acceptor")))))
     (defun ,(intern-system "~a-stop") ()
       (when ,(intern-system "*~a-status*")
         (setf ,(intern-system "*~a-status*") nil)
         (restas-stop (get-config ,(format nil "~a.restas.port" `,system)))))
     (defun ,(intern-system "~a-restart") ()
       (,(intern-system "~a-stop"))
       (,(intern-system "~a-start")))))

(defun restas-stop (port)
  (dolist (acceptor restas::*acceptors*)
    (when (= port (hunchentoot::acceptor-port acceptor))
      (stop acceptor)
      (setf restas::*acceptors* (remove acceptor restas::*acceptors*))))
  (dolist (vhost restas::*vhosts*)
    (when (= port (restas::vhost-port vhost))
      (setf restas::*vhosts* (remove vhost restas::*vhosts*)))))


(defun h-genurl (&rest args)
  (declare (inline))
  (if (boundp '*request*)
      (apply #'genurl (if (parameter "d1m")
                          (append args (list :d1m (parameter "d1m")))
                          args))
      "/"))

(defun h-gen-full-url (&rest args)
  (declare (inline))
  (if (boundp '*request*)
      (apply #'genurl* (if (parameter "d1m")
                                (append args (list :d1m (parameter "d1m")))
                                args))
      "/"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 404
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro m-404 (base-name)
  ;; do *NOT* use with-gensyms here (atleast for x-404), otherwise they get defined in :hawksbill.utils and screw things up
  ;; http://stackoverflow.com/a/5891899
  (let* ((package (symbol-package base-name))
         (r-404 (intern (string-upcase  "r-404") package))
         (v-404 (intern (string-upcase "v-404") package))
         (template (intern (string-upcase "template") package))
         (local-v-404 (intern (string-upcase "local-v-404") package)))
    `(progn
       (def-route ,r-404 ("*any")
         (,v-404))
       (defun ,v-404 ()
         (if (fboundp ',local-v-404)
             (funcall #',local-v-404)
             (,template
              :title "page-not-found"
              :body (<:p :class "error"
                         (translate "404" (<:a :href "javascript:history.go(-1)" (translate "here"))))))))))
