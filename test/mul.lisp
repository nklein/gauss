;;;; test/mul.lisp

(in-package #:gauss-test)

(nst:def-test-group scale-tests ()
  (nst:def-test scale-matrix (:values (:equal 10.0)
                                      (:equal 15.0))
    (let* ((a (gauss:make-vector* '(single-float) 5.0 7.5))
           (b 2.0)
           (c (gauss:scale '(single-float single-float) a b)))
      (values (gauss:vref '(single-float) c 0)
              (gauss:vref '(single-float) c 1)))))

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
              (gauss:vref '(single-float) c 1))))

  (nst:def-test matrix-by-matrix (:values (:equal  6.0) (:equal 17.0)
                                          (:equal 15.0) (:equal 38.0))
    (let* ((a (gauss:make-matrix* '(single-float)
                                  2 3
                                  1.0 2.0 3.0
                                  4.0 5.0 6.0))
           (b (gauss:make-matrix* '(single-float)
                                  3 2
                                  1.0 1.0
                                  1.0 2.0
                                  1.0 4.0))
           (c (gauss:m* '(single-float single-float) a b)))
      (values (gauss:mref '(single-float) c 0 0)
              (gauss:mref '(single-float) c 0 1)
              (gauss:mref '(single-float) c 1 0)
              (gauss:mref '(single-float) c 1 1)))))
