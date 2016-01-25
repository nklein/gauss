;;; src/package.lisp

(defpackage #:gauss
  (:use #:cl)
  (:export #:make-vector
           #:make-vector*
           #:make-matrix
           #:make-matrix*)
  (:export #:mrows
           #:mcols)
  (:export #:mref
           #:vref)
  (:export #:m+
           #:v+)
  (:export #:transpose)
  (:export #:define-matrix-type))
