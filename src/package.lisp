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
  (:export #:transpose)
  (:export #:m+
           #:v+)
  (:export #:m*)
  (:export #:solve)
  (:export #:define-matrix-type))
