;;; src/mul.lisp

(in-package #:gauss)

(template:define-templated-function m* (type-a type-b) (a b)
  `(policy-cond:with-expectations (> speed safety)
       ((assertion (= (mcols a) (mrows b))))
     (make-matrix '(,(contagion-type type-a type-b))
                  (mrows a) (mcols b)
                  (loop :for row :below (mrows a)
                     :appending
                     (loop :for col :below (mcols b)
                        :collect
                        (loop :for k :below (mcols a)
                           :summing
                           (* (mref '(,type-a) a row k)
                              (mref '(,type-b) b k col))))))))
