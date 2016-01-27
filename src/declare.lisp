;;; src/declare.lisp

(in-package #:gauss)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (template:define-template instantiate-matrix-type (type)
    `(progn
       (template:instantiate-templated-function mref ,type)
       (template:instantiate-templated-function set-mref ,type)
       (template:instantiate-templated-function vref ,type)
       (template:instantiate-templated-function vtref ,type)
       (template:instantiate-templated-function set-vref ,type)
       (template:instantiate-templated-function make-matrix ,type)
       (template:instantiate-templated-function make-matrix* ,type)
       (template:instantiate-templated-function make-vector ,type)
       (template:instantiate-templated-function make-vector* ,type)
       (template:instantiate-templated-function transpose ,type)
       (template:instantiate-templated-function m+ ,type ,type)
       (template:instantiate-templated-function v+ ,type ,type)
       (template:instantiate-templated-function m- ,type ,type)
       (template:instantiate-templated-function v- ,type ,type)
       (template:instantiate-templated-function m* ,type ,type)
       (template:instantiate-template make-solver ,type ,type)
       ',type))

  (defmacro define-matrix-type (type)
    `(template:instantiate-template instantiate-matrix-type ,type)))

(define-matrix-type rational)
(define-matrix-type single-float)
(define-matrix-type double-float)
