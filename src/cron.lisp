(in-package :web-utils)

(defmacro cron-restart ()
  `(progn (stop-cron)
          (start-cron)))
