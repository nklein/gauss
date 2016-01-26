;;; src/solve.lisp

(in-package #:gauss)

(template:define-templated-function find-pivot-row (type) (m col)
  `(loop :with best-pivot-row fixnum := col
      :with best-pivot-val ,type := (mref '(,type) m col col)
      :for row fixnum :from (1+ best-pivot-row) :below (mrows m)
      :for val ,type := (mref '(,type) m row col)
      :when (< best-pivot-val val)
        :do (setf best-pivot-row row
                  best-pivot-val val)
      :finally (return best-pivot-row)))

(template:define-templated-function swap-rows-of-matrix (type) (m row-a row-b)
  `(loop :for col fixnum :below (mcols m)
      :for a ,type := (mref '(,type) m row-a col)
      :for b ,type := (mref '(,type) m row-b col)
      :do (set-mref '(,type) b m row-a col)
      :do (set-mref '(,type) a m row-b col)))

(template:define-templated-function row-operation (type)
    (m row row-factor other-row other-row-factor &optional (start 0))
  "Reduce row ROW by taking each element in the row and replacing it
with the element times ROW-FACTOR plus the corresponding element of
the OTHER-ROW times the OTHER-ROW-FACTOR.  Start with index START."
  `(loop :for col :from start :below (mcols m)
      :for r ,type := (mref '(,type) m row col)
      :for o ,type := (mref '(,type) m other-row col)
      :for v ,type := (+ (* r row-factor)
                         (* o other-row-factor))
      :do (set-mref '(,type) v m row col)))

(template:define-templated-function eliminate-column (type) (m v col)
  `(loop :with pivot-row fixnum := col
      :with pivot-value ,type := (- (mref '(,type) m pivot-row col))
      :for row fixnum :from (1+ col) :below (mcols m)
      :for value ,type := (mref '(,type) m row col)
      :do (row-operation '(,type)
                         m row pivot-value pivot-row value col)
      :do (row-operation '(,type)
                         v row pivot-value pivot-row value)))

(template:define-templated-function forward-propagation (type) (m v)
  `(loop :for col :below (mcols m)
      :for pivot-row := (find-pivot-row '(,type) m col)
      :unless (= pivot-row col)
      :do (progn
            (swap-rows-of-matrix '(,type) m pivot-row col)
            (swap-rows-of-matrix '(,type) v pivot-row col))
      :do (eliminate-column '(,type) m v col)))

(template:define-templated-function back-substitute (type) (m v)
  `(loop :for row fixnum :from (1- (mrows m)) :downto 0
      :for pivot ,type := (mref '(,type) m row row)
      :for val ,type :=  (if (zerop pivot)
                             pivot
                             (/ (- (vref '(,type) v row)
                                   (loop :for c :from (1+ row) :below (mcols m)
                                      :summing (the ,type
                                                    (* (mref '(,type) m row c)
                                                       (vref '(,type) v c)))))
                                pivot))
      :do (set-vref '(,type) val v row)))

(template:define-templated-function solve-by-forward-sub-back-prop
    (type) (m v)
  `(progn
     (forward-propagation '(,type) m v)
     (back-substitute '(,type) m v)
     (values v m)))

(template:define-templated-function solve (type-m type-v) (m v)
  (let ((new-type-v (contagion-type type-m type-v)))
    `(policy-cond:with-expectations (> speed safety)
         ((assertion (= (mcols m) (mrows v)))
          (assertion (= (mrows m) (mcols m))))
       (let ((m (copy-matrix m ',new-type-v))
             (v (copy-matrix v ',new-type-v)))
         (values (solve-by-forward-sub-back-prop '(,new-type-v) m v))))))

(template:define-template make-solver (type-m type-v)
  (let ((type (contagion-type type-m type-v)))
    `(progn
       (template:instantiate-templated-function find-pivot-row ,type)
       (template:instantiate-templated-function swap-rows-of-matrix ,type)
       (template:instantiate-templated-function row-operation ,type)
       (template:instantiate-templated-function eliminate-column ,type)
       (template:instantiate-templated-function forward-propagation ,type)
       (template:instantiate-templated-function back-substitute ,type)
       (template:instantiate-templated-function solve-by-forward-sub-back-prop
                                                ,type)
       (template:instantiate-templated-function solve ,type-m ,type-v))))
