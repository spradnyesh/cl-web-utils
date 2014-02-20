(defpackage :web-utils-tests-system
  (:use :cl :asdf))

(in-package :web-utils-tests-system)

(defsystem web-utils-tests
  :components ((:module "tests"
                        :components ((:file "package")
                                     (:file "cipher" :depends-on ("package"))
                                     (:file "ckeditor" :depends-on ("package"))
                                     (:file "cron" :depends-on ("package"))
                                     (:file "datetime" :depends-on ("package"))
                                     (:file "db" :depends-on ("package"))
                                     (:file "file" :depends-on ("package"))
                                     (:file "html" :depends-on ("package"))
                                     (:file "list" :depends-on ("package"))
                                     (:file "string" :depends-on ("package"))
                                     (:file "memoize" :depends-on ("package")))))
  :depends-on (:web-utils :fiveam))
