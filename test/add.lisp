;;;; test/add.lisp

(in-package #:gauss-test)

(nst:def-test-group add-tests ()
  (nst:def-test simple-two-by-one (:values (:equal 3.0)
                                           (:equal 6.0))
    (let* ((a (gauss:make-vector* '(single-float) 1.0 2.0))
           (b (gauss:make-vector* '(single-float) 2.0 4.0))
           (c (gauss:m+ '(single-float single-float) a b)))
      (values (gauss:vref '(single-float) c 0)
              (gauss:vref '(single-float) c 1)))))
