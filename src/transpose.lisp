;;; src/transpose.lisp

(in-package #:gauss)

(template:define-templated-function transpose (type) (a)
  `(policy-cond:with-expectations (> speed safety)
       ((assertion (eql ',type (mtype a))))
     (%matrix :rows (mcols a) :cols (mrows a)
              :cf (mrf a) :rf (mcf a)
              :vals (mvals a))))
