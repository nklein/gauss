(load "gauss.asd")
(ql:quickload :gauss)

(template:define-templated-function make-random-matrix (type) (n)
  `(let ((vals (loop :repeat (* n n)
                  :collect (+ 0.1 (random (coerce 1.0 ',type))))))
     (gauss:make-matrix '(,type) n n vals)))

(template:define-templated-function make-nonsingular-matrix (type) (n)
  `(let ((vals (loop :for r :below n
                  :appending (loop :with v := (coerce (/ r 10) ',type)
                                :for c :below n
                                :collecting (expt c v)))))
     (gauss:make-matrix '(,type) n n vals)))

(template:define-templated-function make-random-vector (type) (n)
  `(let ((vals (loop :repeat n
                  :collect (random (coerce 0.01 ',type)))))
     (gauss:make-vector '(,type) vals)))

(template:instantiate-templated-function make-random-matrix single-float)
(template:instantiate-templated-function make-nonsingular-matrix single-float)
(template:instantiate-templated-function make-random-vector single-float)

(defmacro bench (op type &optional (iterations 10000))
  (let ((a (gensym "A"))
        (b (gensym "B")))
    `(let ((,a (make-random-matrix '(,type) 50))
           (,b (make-random-matrix '(,type) 50)))
       (time
        (loop :repeat ,iterations
           :do (,op '(,type ,type) ,a ,b))))))

(bench gauss:m+ single-float 1000)
#+not
(bench gauss:m* single-float 1000)

(defmacro bench-solve (type &optional (iterations 10000))
  (let ((a (gensym "A"))
        (b (gensym "B")))
    `(let ((,a (make-nonsingular-matrix '(,type) 25))
           (,b (make-random-vector '(,type) 25)))
       (time
        (loop :repeat ,iterations
           :do (gauss:solve '(,type ,type) ,a ,b))))))

(bench-solve single-float 1000)
(gauss:solve '(single-float single-float)
             (make-nonsingular-matrix '(single-float) 25)
             (make-random-vector '(single-float) 25))
