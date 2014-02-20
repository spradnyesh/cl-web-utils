(in-package :web-utils-tests)

(def-suite :cron :in :web-utils)
(in-suite :cron)

;; cron-restart
(test cron-restart
  (is (equal (macroexpand-1 '(cron-restart))
             '(PROGN (CL-CRON:STOP-CRON) (CL-CRON:START-CRON)))))
