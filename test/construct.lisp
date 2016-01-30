;;;; test/construct.lisp

(in-package #:gauss-test)

(nst:def-test-group construct-tests ()
  (nst:def-test simple-2d-vector (:values (:equal 1.0)
                                          (:equal 2.0))
    (let ((v (gauss:make-vector* '(single-float) 1.0 2.0)))
      (values (gauss:vref '(single-float) v 0)
              (gauss:vref '(single-float) v 1))))

  (nst:def-test simple-2d-vector-from-list (:values (:equal 1.0)
                                          (:equal 2.0))
    (let ((v (gauss:make-vector '(single-float) '(1.0 2.0))))
      (values (gauss:vref '(single-float) v 0)
              (gauss:vref '(single-float) v 1))))

  (nst:def-test simple-two-by-two (:values (:equal 1.0) (:equal 2.0)
                                           (:equal 4.0) (:equal 8.0))
    (let ((m (gauss:make-matrix* '(single-float) 2 2
                                 1.0 2.0
                                 4.0 8.0)))
      (values (gauss:mref '(single-float) m 0 0)
              (gauss:mref '(single-float) m 0 1)
              (gauss:mref '(single-float) m 1 0)
              (gauss:mref '(single-float) m 1 1))))

  (nst:def-test simple-two-by-two-from-list (:values (:equal 1.0) (:equal 2.0)
                                                     (:equal 4.0) (:equal 8.0))
    (let ((m (gauss:make-matrix '(single-float) 2 2
                                '(1.0 2.0
                                  4.0 8.0))))
      (values (gauss:mref '(single-float) m 0 0)
              (gauss:mref '(single-float) m 0 1)
              (gauss:mref '(single-float) m 1 0)
              (gauss:mref '(single-float) m 1 1)))))


(nst:def-test-group construct-shortcut-tests ()
  (nst:def-test simple-2d-vector/s (:values (:equal 1.0)
                                            (:equal 2.0))
    (let ((v (gauss:make-vector*/s 1.0 2.0)))
      (values (gauss:vref/s v 0)
              (gauss:vref/s v 1))))

  (nst:def-test simple-2d-vector-from-list/s (:values (:equal 1.0)
                                                      (:equal 2.0))
    (let ((v (gauss:make-vector/s '(1.0 2.0))))
      (values (gauss:vref/s v 0)
              (gauss:vref/s v 1))))

  (nst:def-test simple-two-by-two/s (:values (:equal 1.0) (:equal 2.0)
                                             (:equal 4.0) (:equal 8.0))
    (let ((m (gauss:make-matrix*/s 2 2
                                   1.0 2.0
                                   4.0 8.0)))
      (values (gauss:mref/s m 0 0)
              (gauss:mref/s m 0 1)
              (gauss:mref/s m 1 0)
              (gauss:mref/s m 1 1))))

  (nst:def-test simple-two-by-two-from-list/s
      (:values (:equal 1.0) (:equal 2.0)
               (:equal 4.0) (:equal 8.0))
    (let ((m (gauss:make-matrix/s 2 2
                                  '(1.0 2.0
                                    4.0 8.0))))
      (values (gauss:mref/s m 0 0)
              (gauss:mref/s m 0 1)
              (gauss:mref/s m 1 0)
              (gauss:mref/s m 1 1)))))
