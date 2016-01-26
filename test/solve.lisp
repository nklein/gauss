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
              (gauss:vref '(single-float) c 1))))

  ;; [ 1 1 ] [ x ]   [ 1 ]
  ;; [ 0 1 ] [ y ] = [ 2 ]
  ;;
  ;;  y = 2
  ;;  x + y = 1
  ;;  x = -1
  (nst:def-test solve-upper-triangular (:values (:equal -1.0)
                                                (:equal  2.0))
    (let* ((a (gauss:make-matrix* '(single-float)
                                  2 2
                                  1.0 1.0
                                  0.0 1.0))
           (b (gauss:make-vector* '(single-float) 1.0 2.0))
           (c (gauss:solve '(single-float single-float) a b)))
      (values (gauss:vref '(single-float) c 0)
              (gauss:vref '(single-float) c 1))))

  ;; [ 0 1 ] [ x ] = [ 1 ]
  ;; [ 1 1 ] [ y ] = [ 2 ]
  ;;  x = 1
  ;;  x + y = 2
  ;;  y = 1
  (nst:def-test solve-lower-triangular (:values (:equal 1.0)
                                                (:equal 1.0))
    (let* ((a (gauss:make-matrix* '(single-float)
                                  2 2
                                  0.0 1.0
                                  1.0 1.0))
           (b (gauss:make-vector* '(single-float) 1.0 2.0))
           (c (gauss:solve '(single-float single-float) a b)))
      (values (gauss:vref '(single-float) c 0)
              (gauss:vref '(single-float) c 1))))

  ;; [ 1 2 ] [ x ]   [ 1 ]
  ;; [ 3 4 ] [ y ] = [ 2 ]
  ;;
  ;; 3x + 4y = 2
  ;;  x + 2y = 1
  ;;  2y = 1 - x
  ;; 3x + 2(1-x) = 2
  ;; 3x - 2x = 0
  ;; x = 0
  ;; y = 1/2
  (nst:def-test solve-general (:values (:equal 0.0)
                                       (:equal 0.5))
    (let* ((a (gauss:make-matrix* '(single-float)
                                  2 2
                                  1.0 2.0
                                  3.0 4.0))
           (b (gauss:make-vector* '(single-float) 1.0 2.0))
           (c (gauss:solve '(single-float single-float) a b)))
      (values (gauss:vref '(single-float) c 0)
              (gauss:vref '(single-float) c 1)))))
