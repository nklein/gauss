;;; src/contagion.lisp

(in-package #:gauss)

(defgeneric contagion-type (a b)
  (:method ((a (eql 'double-float)) (b (eql 'double-float)))
    'double-float)
  (:method ((a (eql 'double-float)) (b (eql 'single-float)))
    'double-float)
  (:method ((a (eql 'single-float)) (b (eql 'double-float)))
    'double-float)
  (:method ((a (eql 'double-float)) (b (eql 'rational)))
    'double-float)
  (:method ((a (eql 'rational)) (b (eql 'double-float)))
    'double-float)

  (:method ((a (eql 'single-float)) (b (eql 'single-float)))
    'single-float)
  (:method ((a (eql 'single-float)) (b (eql 'rational)))
    'single-float)
  (:method ((a (eql 'rational)) (b (eql 'single-float)))
    'single-float)

  (:method ((a (eql 'rational)) (b (eql 'rational)))
    'rational))
