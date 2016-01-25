;;; src/class.lisp

(in-package #:gauss)

(template:define-templated-function make-matrix (type) (rows cols &rest vals)
  `(policy-cond:with-expectations (> speed safety)
       ((type (unsigned-byte 1 *) rows)
        (type (unsigned-byte 1 *) cols)
        (assertion (= (length vals) (* rows cols))))
     (make-array (list rows cols)
                 :element-type ',type
                 :initial-contents vals)))
