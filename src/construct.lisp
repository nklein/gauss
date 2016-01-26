;;; src/construct.lisp

(in-package #:gauss)

(declaim (inline mrows mcols mvals mtype))
(defstruct (matrix (:conc-name "M")
                   (:constructor %matrix))
  (rows 0 :type (integer 1 *) :read-only t)
  (cols 0 :type (integer 1 *) :read-only t)
  (cf 0 :type (integer 1 *) :read-only t)
  (rf 0 :type (integer 1 *) :read-only t)
  (vals 0 :read-only t))

(defun copy-matrix (m)
  (check-type m matrix)
  (%matrix :rows (mrows m) :cols (mcols m)
           :cf (mcf m) :rf (mrf m)
           :vals (copy-seq (mvals m))))

(defun mtype (m)
  (check-type m matrix)
  (array-element-type (mvals m)))

(template:define-templated-function make-matrix (type) (rows cols vals)
  `(policy-cond:with-expectations (> speed safety)
       ((type (integer 1 *) rows)
        (type (integer 1 *) cols)
        (assertion (= (length vals) (* rows cols)))
        (assertion (every (lambda (v)
                            (typep v ',type))
                          vals)))
     (let ((vals-array (make-array (* rows cols)
                                   :element-type ',type
                                   :initial-contents vals)))
       (%matrix :rows rows :cols cols
                :cf 1 :rf cols
                :vals vals-array))))

(template:define-templated-function make-matrix* (type) (rows cols &rest vals)
  `(make-matrix '(,type) rows cols vals))

(template:define-templated-function make-vector (type) (vals)
  `(make-matrix '(,type) (length vals) 1 vals))

(template:define-templated-function make-vector* (type) (&rest vals)
  `(make-vector '(,type) vals))
