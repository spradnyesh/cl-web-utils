(in-package :hawksbill.utils)

(defmacro sendmail (&key to subject cc bcc attach body package)
  (declare (ignore attach))
  (let ((template (intern (string-upcase "template") (symbol-package package))))
    `(send-email (get-config "site.email.host")
                 (get-config "site.email.address")
                 ,to
                 ,subject
                 (,template :title (translate "confirm-registration-email-header")
                            :body ,body
                            :email t)
                 :cc ,cc
                 :bcc ,bcc)))
