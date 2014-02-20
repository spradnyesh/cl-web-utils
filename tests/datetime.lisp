(in-package :web-utils-tests)

(def-suite :datetime :in :web-utils)
(in-suite :datetime)

;; prettyprint-date
(test prettyprint-date
  (is (string-equal (prettyprint-date (local-time:encode-timestamp 0 0 0 0 20 02 2014))
                    "Thu, Feb 20 2014")))

;; prettyprint-time
(test prettyprint-time
  (is (string-equal (prettyprint-time (local-time:encode-timestamp 0 57 12 18 20 02 2014))
                    "6:12 pm")))
