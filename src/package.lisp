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
  (:export #:scale
           #:m*)
  (:export #:solve)
  (:export #:define-matrix-type
           #:define-mixed-type-matrix-operations)
  (:export #:define-matrix-operation-shortcuts)
  (:export #:make-vector/q #:make-vector*/q
           #:make-matrix/q #:make-matrix*/q
           #:mref/q #:vref/q #:transpose/q
           #:m+/qq #:v+/qq #:m-/qq #:v-/qq
           #:scale/qq #:m*/qq
           #:solve/qq)
  (:export #:make-vector/s #:make-vector*/s
           #:make-matrix/s #:make-matrix*/s
           #:mref/s #:vref/s #:transpose/s
           #:m+/ss #:v+/ss #:m-/ss #:v-/ss
           #:scale/ss #:m*/ss
           #:solve/ss)
  (:export #:make-vector/d #:make-vector*/d
           #:make-matrix/d #:make-matrix*/d
           #:mref/d #:vref/d #:transpose/d
           #:m+/dd #:v+/dd #:m-/dd #:v-/dd
           #:scale/dd #:m*/dd
           #:solve/dd))
