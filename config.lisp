(in-package :hawksbill.utils)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; config classes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass config ()
  ((name :initarg :name :initform nil :accessor name)
   (value :initarg :value :initform nil :accessor value)))
(defclass config-node ()
  ((dimension :initarg :dimension :initform nil :accessor dimension)
   (value :initarg :value :initform nil :accessor value)))
(defclass config-storage ()
  ((configs :initform nil :accessor configs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; helper functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun is-valid-dimension-value (dimension value)
  (not (set-difference (list value)
                       (handler-case (symbol-value (intern (string-upcase (format nil
                                                                      "*valid-~as*"
                                                                      dimension))))
                         (unbound-variable () (return-from is-valid-dimension-value nil)))
                       :test #'string-equal)))

(defun is-valid-dimensions-map (dimensions-map)
  (if (and (= 1 (length dimensions-map))
           (equal "master" (first (first dimensions-map))))
      t
      (dolist (dm dimensions-map)
        (unless (is-valid-dimension-value (first dm) (second dm))
          (return-from is-valid-dimensions-map nil))))
  t)

(defun get-dimensions-string (dimensions-map)
  (join-string-list-with-delim ","
                               (let ((rslt nil))
                                 (dolist (dm dimensions-map)
                                   (push (join-string-list-with-delim ":" dm) rslt))
                                 (nreverse rslt))))

(defun get-dimensions-map (dimensions-string)
  (let ((rslt nil))
    (dolist (dl (split-sequence "," dimensions-string :test #'string-equal))
      (let ((d (split-sequence ":" dl :test #'string-equal)))
        (push d rslt)))
    (nreverse rslt)))

(defun reduce-dimensions-map (dimensions-map)
  (nreverse (rest (nreverse dimensions-map))))

(defun reduce-dimensions-string (dimensions-string)
  (get-dimensions-string (reduce-dimensions-map (get-dimensions-map dimensions-string))))

(defun get-config-helper (name dimensions-map config-storage)
  (when dimensions-map
    (let ((config-node (if dimensions-map
                           (find (get-dimensions-string dimensions-map)
                                 (configs config-storage)
                                 :key #'dimension
                                 :test #'string-equal)
                           (find "master"
                                 (configs config-storage)
                                 :key #'dimension
                                 :test #'string-equal))))
      (if config-node
          (let ((value (find name
                             (value config-node)
                             :key #'name
                             :test #'string-equal)))
            (if value
                (value value)
                (get-config-helper name (reduce-dimensions-map dimensions-map) config-storage)))
          (get-config-helper name (reduce-dimensions-map dimensions-map) config-storage)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; get/add/show/init functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun get-config (name &optional (dimensions-string *current-dimensions-string*) (config-storage *config-storage*))
  (get-config-helper name (get-dimensions-map dimensions-string) config-storage))

(defun add-config (name value dimensions-string &optional (config-storage *config-storage*))
  "does _not_ check for duplicates while adding; due to _push_, get-config will always get the latest value => the older values just increase the size, but that's nominal, and hence ok ;)"
  (when (is-valid-dimensions-map (get-dimensions-map dimensions-string))
    (let ((config (make-instance 'config :name name :value value))
          (config-node (find dimensions-string
                             (configs config-storage)
                             :key #'dimension
                             :test #'string-equal)))
      (if config-node
          (push config (value config-node))
          (push (make-instance 'config-node :dimension dimensions-string :value (list config))
                (configs config-storage))))))

(defun show-config-tree (&optional (config-storage *config-storage*))
  (dolist (cn (configs config-storage))
    (format t "***** ~a *****~%" (dimension cn))
    (dolist (v (value cn))
      (format t "~a: ~a~%" (name v) (value v)))))

(defun init-config-tree (&optional (config *config*) (config-storage *config-storage*))
  "input: '(('envt:dev' ('n1' 'v1') ('n2' 'v2')) ('envt:prod' ('n3' 'v3') ('n4' 'v4')))"
  (dolist (c config)
    (let ((dimension-string (first c)))
      (dolist (name-value (rest c))
        (if (= 2 (length name-value))
            (add-config (first name-value)
                        (second name-value)
                        dimension-string
                        config-storage)
            (let ((module-name (first name-value)))
              (dolist (module (rest name-value))
                (add-config (format nil "~a.~a" module-name (first module))
                            (second module)
                            dimension-string
                            config-storage))))))))
