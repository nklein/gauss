;;;; test/transpose.lisp

(in-package #:gauss-test)

(nst:def-test-group transpose-tests ()
  (nst:def-test transpose-vector (:values (:equal 1) (:equal 2))
    (let* ((a (gauss:make-vector* '(single-float) 3.0 4.0))
           (b (gauss:transpose '(single-float) a)))
      (values (gauss:mrows b)
              (gauss:mcols b))))

  (nst:def-test transpose-two-by-two (:values (:equal 1.0) (:equal 3.0)
                                              (:equal 2.0) (:equal 4.0))
    (let* ((a (gauss:make-matrix* '(single-float)
                                  2 2
                                  1.0 2.0
                                  3.0 4.0))
           (b (gauss:transpose '(single-float) a)))
      (values (gauss:mref '(single-float) b 0 0)
              (gauss:mref '(single-float) b 0 1)
              (gauss:mref '(single-float) b 1 0)
              (gauss:mref '(single-float) b 1 1)))))
