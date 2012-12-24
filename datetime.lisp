(in-package :hawksbill.utils)

(defun prettyprint-datetime (&optional (timestamp (now)))
  (format-timestring nil timestamp :format '(:short-weekday ", " :short-month " " :day ", " :year " at " :hour12 ":" :min " " :ampm)))
