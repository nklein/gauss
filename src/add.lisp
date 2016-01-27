;;; src/add.lisp

(in-package #:gauss)

(template:define-templated-function m+ (type-a type-b) (a b)
  `(policy-cond:with-expectations (> speed safety)
       ((assertion (= (mrows a) (mrows b)))
        (assertion (= (mcols a) (mcols b))))
     (make-matrix '(,(contagion-type type-a type-b))
                  (mrows a) (mcols a)
                  (loop :with av := (mvals a)
                     :with bv := (mvals b)
                     :for i :below (* (mrows a) (mcols a))
                     :collecting (+ (the ,type-a (aref av i))
                                    (the ,type-b (aref bv i)))))))

(template:define-templated-function v+ (type-a type-b) (a b)
  `(m+ '(,type-a ,type-b) a b))

(template:define-templated-function m- (type-a type-b) (a b)
  `(policy-cond:with-expectations (> speed safety)
       ((assertion (= (mrows a) (mrows b)))
        (assertion (= (mcols a) (mcols b))))
     (make-matrix '(,(contagion-type type-a type-b))
                  (mrows a) (mcols a)
                  (loop :with av := (mvals a)
                     :with bv := (mvals b)
                     :for i :below (* (mrows a) (mcols a))
                     :collecting (- (the ,type-a (aref av i))
                                    (the ,type-b (aref bv i)))))))

(template:define-templated-function v- (type-a type-b) (a b)
  `(m- '(,type-a ,type-b) a b))
