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
              (gauss:vref '(single-float) c 1))))

  (nst:def-test solve-general-matrix (:values (:equal 0.0) (:equal 0.0)
                                              (:equal 0.5) (:equal 1.0))
    (let* ((a (gauss:make-matrix* '(single-float)
                                  2 2
                                  1.0 2.0
                                  3.0 4.0))
           (b (gauss:make-matrix* '(single-float)
                                  2 2
                                  1.0 2.0
                                  2.0 4.0))
           (c (gauss:solve '(single-float single-float) a b)))
      (values (gauss:mref '(single-float) c 0 0)
              (gauss:mref '(single-float) c 0 1)
              (gauss:mref '(single-float) c 1 0)
              (gauss:mref '(single-float) c 1 1))))

  ;;  [ 1 1 1 1 ] [ 1 ]   [ 10 ]
  ;;  [ 1 1 0 1 ] [ 2 ] = [  7 ]
  ;;  [ 1 0 1 1 ] [ 3 ]   [  8 ]
  ;;  [ 1 2 3 4 ] [ 4 ]   [ 30 ]

  (nst:def-test solve-general-4-by-4 (:values (:equal 1.0)
                                              (:equal 2.0)
                                              (:equal 3.0)
                                              (:equal 4.0))
    (let* ((a (gauss:make-matrix* '(single-float)
                                  4 4
                                  1.0 1.0 1.0 1.0
                                  1.0 1.0 0.0 1.0
                                  1.0 0.0 1.0 1.0
                                  1.0 2.0 3.0 4.0))
           (b (gauss:make-vector* '(single-float) 10.0 7.0 8.0 30.0))
           (c (gauss:solve '(single-float single-float) a b)))
      (values (gauss:vref '(single-float) c 0)
              (gauss:vref '(single-float) c 1)
              (gauss:vref '(single-float) c 2)
              (gauss:vref '(single-float) c 3)))))

(nst:def-test-group solve-shortcut-tests ()
  (nst:def-test solve-identity/ss (:values (:equal 1.0)
                                           (:equal 2.0))
    (let* ((a (gauss:make-matrix*/s 2 2
                                    1.0 0.0
                                    0.0 1.0))
           (b (gauss:make-vector*/s 1.0 2.0))
           (c (gauss:solve/ss a b)))
      (values (gauss:vref/s c 0)
              (gauss:vref/s c 1))))

  ;; [ 1 1 ] [ x ]   [ 1 ]
  ;; [ 0 1 ] [ y ] = [ 2 ]
  ;;
  ;;  y = 2
  ;;  x + y = 1
  ;;  x = -1
  (nst:def-test solve-upper-triangular/ss (:values (:equal -1.0)
                                                   (:equal  2.0))
    (let* ((a (gauss:make-matrix*/s 2 2
                                    1.0 1.0
                                    0.0 1.0))
           (b (gauss:make-vector*/s 1.0 2.0))
           (c (gauss:solve/ss a b)))
      (values (gauss:vref/s c 0)
              (gauss:vref/s c 1))))

  ;; [ 0 1 ] [ x ] = [ 1 ]
  ;; [ 1 1 ] [ y ] = [ 2 ]
  ;;  x = 1
  ;;  x + y = 2
  ;;  y = 1
  (nst:def-test solve-lower-triangular/ss (:values (:equal 1.0)
                                                   (:equal 1.0))
    (let* ((a (gauss:make-matrix*/s 2 2
                                    0.0 1.0
                                    1.0 1.0))
           (b (gauss:make-vector*/s 1.0 2.0))
           (c (gauss:solve/ss a b)))
      (values (gauss:vref/s c 0)
              (gauss:vref/s c 1))))

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
  (nst:def-test solve-general/ss (:values (:equal 0.0)
                                          (:equal 0.5))
    (let* ((a (gauss:make-matrix*/s 2 2
                                    1.0 2.0
                                    3.0 4.0))
           (b (gauss:make-vector*/s 1.0 2.0))
           (c (gauss:solve/ss a b)))
      (values (gauss:vref/s c 0)
              (gauss:vref/s c 1))))

  (nst:def-test solve-general-matrix/ss (:values (:equal 0.0) (:equal 0.0)
                                                 (:equal 0.5) (:equal 1.0))
    (let* ((a (gauss:make-matrix*/s 2 2
                                    1.0 2.0
                                    3.0 4.0))
           (b (gauss:make-matrix*/s 2 2
                                    1.0 2.0
                                    2.0 4.0))
           (c (gauss:solve/ss a b)))
      (values (gauss:mref/s c 0 0)
              (gauss:mref/s c 0 1)
              (gauss:mref/s c 1 0)
              (gauss:mref/s c 1 1))))

  ;;  [ 1 1 1 1 ] [ 1 ]   [ 10 ]
  ;;  [ 1 1 0 1 ] [ 2 ] = [  7 ]
  ;;  [ 1 0 1 1 ] [ 3 ]   [  8 ]
  ;;  [ 1 2 3 4 ] [ 4 ]   [ 30 ]

  (nst:def-test solve-general-4-by-4/ss (:values (:equal 1.0)
                                                 (:equal 2.0)
                                                 (:equal 3.0)
                                                 (:equal 4.0))
    (let* ((a (gauss:make-matrix*/s 4 4
                                    1.0 1.0 1.0 1.0
                                    1.0 1.0 0.0 1.0
                                    1.0 0.0 1.0 1.0
                                    1.0 2.0 3.0 4.0))
           (b (gauss:make-vector*/s 10.0 7.0 8.0 30.0))
           (c (gauss:solve/ss a b)))
      (values (gauss:vref/s c 0)
              (gauss:vref/s c 1)
              (gauss:vref/s c 2)
              (gauss:vref/s c 3)))))
