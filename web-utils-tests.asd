(defpackage :web-utils-tests-system
  (:use :cl :asdf))

(in-package :web-utils-tests-system)

(defsystem web-utils-tests
  :components ((:module "tests"
                        :components ((:file "package")
                                     (:file "list" :depends-on ("package")))))
  :depends-on (:web-utils :fiveam))
