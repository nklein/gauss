;;; src/package.lisp

(defpackage #:gauss
  (:use #:cl)
  (:export #:matrix
           #:make-vector
           #:make-vector*
           #:make-matrix
           #:make-matrix*)
  (:export #:mrows
           #:mcols
           #:mtype)
  (:export #:mref
           #:vref)
  (:export #:transpose)
  (:export #:square-matrix-p
           #:commensuratep
           #:column-vector-p)
  (:export #:m+
           #:v+
           #:m-
           #:v-)
  (:export #:m*)
  (:export #:solve)
  (:export #:define-matrix-type))
