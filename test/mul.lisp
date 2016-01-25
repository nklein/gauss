;;;; test/mul.lisp

(in-package #:gauss-test)

(nst:def-test-group mul-tests ()
  (nst:def-test simple-matrix-by-vector (:values (:equal 5.0)
                                                 (:equal 11.0))
    (let* ((a (gauss:make-matrix* '(single-float)
                                  2 2
                                  1.0 2.0
                                  3.0 4.0))
           (b (gauss:make-vector* '(single-float) 1.0 2.0))
           (c (gauss:m* '(single-float single-float) a b)))
      (values (gauss:vref '(single-float) c 0)
              (gauss:vref '(single-float) c 1)))))
