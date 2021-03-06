(in-package :web-utils)

(defun paginate (list offset limit)
  (when list
    (let* ((list (if (consp list)
                     list
                     (list list)))
           (len (length list))
           (end (+ limit offset)))
      (if (and (not (minusp offset))
               (> len offset))
          (subseq list
                  offset
                  (if (and list (< end len))
                      end))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; pagination markup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; page-number: current page number
;; num-pages: max #pages that pagination markup can show
;; max-pages: max page-number that can be shown (basically, (/ #max-items num-pages))

(defun pagination-low (page-number num-pages)
  "page number can be 0 at minimum"
  (max (- page-number (/ num-pages 2))
       0))

(defun pagination-high (page-number max-pages num-pages)
  "page number can be max-pages at maximum"
  (min (+ page-number (/ num-pages 2))
       max-pages))

(defun pagination-combine-low-high (page-number max-pages num-pages)
  (let ((low (pagination-low page-number num-pages))
        (high (pagination-high page-number max-pages num-pages))
        (ideal-low (- page-number (/ num-pages 2)))
        (ideal-high (+ page-number (/ num-pages 2)))
        (diff 0))
    (when (and (/= (- high low) (1+ num-pages))
               (>= max-pages num-pages)) ; if max-pages <= num-pages, do nothing
      (if (> low ideal-low)
          ;; adjust high, but it should still be < max-pages
          (progn
            (setf diff (- low ideal-low))
            (when (<= (+ high diff) max-pages)
              (setf high (+ high diff))))
          ;; adjust low, but it should still be > 0
          (progn
            (setf diff (- ideal-high high))
            (when (>= (- low diff) 0)
              (setf low (- low diff))))))
    (list low high)))

(defmacro pagination-markup (page-number max-results num-per-page num-pages route &rest route-params)
  "build URLs b/n route?p=low and route?p=high"
  `(let* ((max-pages (floor (/ (1- ,max-results) ,num-per-page)))
          (low-high (pagination-combine-low-high ,page-number max-pages ,num-pages)))
     (<:div :class "pagination"
      ;; do NOT show pagination-markup
      ;; when page-number = 13, *article-num-per-page* = 10 and max-results = 100 ;)
      ;; OR when max-results < 10
      (when (and (<= ,page-number max-pages)
                 (> ,max-results ,num-per-page))
        (<:ol
              (if (= ,page-number 0)
                  (<:li :class "disabled" "first")
                  (<:li (<:a :href (h-genurl ,route ,@route-params :page 0) "first")))
              (if (< (- ,page-number 10) 0)
                  (<:li :class "disabled" "-10")
                  (<:li (<:a :href (h-genurl ,route ,@route-params :page (- ,page-number 10)) "-10")))
              (if (= ,page-number 0)
                  (<:li :class "disabled" "prev")
                  (<:li (<:a :href (h-genurl ,route ,@route-params :page (1- ,page-number)) "prev")))
              (loop for i from (first low-high) to (second low-high)
                 collecting (if (= ,page-number i)
                                (<:li :class "disabled" i)
                                (<:li (<:a :href (h-genurl ,route ,@route-params :page i) i)))
                   into a
                   finally (return (apply #'concatenate 'string a)))
              (if (= ,page-number max-pages)
                  (<:li :class "disabled" "next")
                  (<:li (<:a :href (h-genurl ,route ,@route-params :page (1+ ,page-number)) "next")))
              (if (> (+ ,page-number 10) max-pages)
                  (<:li :class "disabled" "+10")
                  (<:li (<:a :href (h-genurl ,route ,@route-params :page (+ ,page-number 10)) "+10")))
              (if (= ,page-number max-pages)
                  (<:li :class "disabled" "last")
                  (<:li (<:a :href (h-genurl ,route ,@route-params :page max-pages) "last")))))
      (<:p :class "pagination-results small" (format nil "~a results" ,max-results)))))
