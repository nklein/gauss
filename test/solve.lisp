;;;; test/solve.lisp

(in-package #:gauss-test)

(nst:def-test-group solve-tests ()
  (nst:def-test solve-identity (:values (:equal 1.0)
                                        (:equal 2.0))
    (let* ((a (gauss:make-matrix* '(single-float)
                                  2 2
                                  1.0 0.0
                                  0.0 1.0))
           (b (gauss:make-vector* '(single-float) 1.0 2.0))
           (c (gauss:solve '(single-float single-float) a b)))
      (values (gauss:vref '(single-float) c 0)
              (gauss:vref '(single-float) c 1)))))
