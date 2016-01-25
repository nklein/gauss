(load "gauss.asd")
(ql:quickload :gauss)

(template:define-templated-function make-random-matrix (type) (n)
  `(let ((vals (loop :repeat (* n n)
                  :collect (random (coerce 1 ',type)))))
     (gauss:make-matrix '(,type) n n vals)))

(template:instantiate-templated-function make-random-matrix single-float)
(template:instantiate-templated-function make-random-matrix double-float)

(defmacro bench (op type &optional (iterations 10000))
  (let ((a (gensym "A"))
        (b (gensym "B")))
    `(let ((,a (make-random-matrix '(,type) 50))
           (,b (make-random-matrix '(,type) 50)))
       (time
        (loop :repeat ,iterations
           :do (setf ,a (,op '(,type ,type) ,a ,b)))))))

(bench gauss:m+ single-float 1000)
(bench gauss:m+ double-float 1000)
