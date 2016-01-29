;;; src/declare.lisp

(in-package #:gauss)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (template:define-template instantiate-mixed-type-matrix-operations
      (type-a type-b)
    `(progn
       (template:instantiate-templated-function m+ ,type-a ,type-b)
       (template:instantiate-templated-function v+ ,type-a ,type-b)
       (template:instantiate-templated-function m- ,type-a ,type-b)
       (template:instantiate-templated-function v- ,type-a ,type-b)
       (template:instantiate-templated-function scale ,type-a ,type-b)
       (template:instantiate-templated-function m* ,type-a ,type-b)
       (template:instantiate-template make-solver ,type-a ,type-b)
       (list ',type-a ',type-b)))

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
       (template:instantiate-template instantiate-mixed-type-matrix-operations
                                      ,type ,type)
       ',type))

  (defmacro define-matrix-type (type)
    `(template:instantiate-template instantiate-matrix-type ,type))

  (defmacro define-mixed-type-matrix-operations (type-a type-b)
    `(progn
       (template:instantiate-template instantiate-mixed-type-matrix-operations
                                      ,type-a ,type-b)
       (template:instantiate-template instantiate-mixed-type-matrix-operations
                                      ,type-b ,type-a))))

(locally (declare (optimize (speed 2) (safety 3)))
  (define-matrix-type rational)
  (define-matrix-type single-float)
  (define-matrix-type double-float))
