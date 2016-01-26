;;; src/solve.lisp

(in-package #:gauss)

(template:define-templated-function solve-by-gaussian-elimination
    (type) (m v)
  `(flet ((find-pivot-row (col)
            (loop :with best-pivot-row fixnum := col
               :with best-pivot-val ,type := (mref '(,type) m col col)
               :for row fixnum :from (1+ best-pivot-row) :below (mrows m)
               :for val ,type := (mref '(,type) m row col)
               :when (< best-pivot-val val)
                 :do (setf best-pivot-row row
                           best-pivot-val val)
               :finally (return best-pivot-row)))

          (swap-rows-of-matrix (row-a row-b)
            (let ((vals (mvals m))
                  (mrf (mrf m))
                  (mcf (mcf m)))
              (declare (type (array ,type (*)) vals)
                       (type fixnum mrf mcf))
              (loop :with mrf-a fixnum := (* mrf row-a)
                    :with mrf-b fixnum := (* mrf row-b)
                 :for col :below (mcols m)
                 :for a := (+ mrf-a (* mcf col))
                 :for b := (+ mrf-b (* mcf col))
                 :do (rotatef (aref vals a) (aref vals b)))))

          (swap-rows-of-vector (row-a row-b)
            (let ((vals (mvals v)))
              (declare (type (array ,type) vals))
              (rotatef (aref vals row-a) (aref vals row-b))))

          (eliminate-column (col)
            (let ((p (- (mref '(,type) m col col)))
                  (vals (mvals m))
                  (vvals (mvals v))
                  (mrf (mrf m))
                  (mcf (mcf m)))
              (loop :for row :from (1+ col) :below (mcols m)
                 :for q := (mref '(,type) m row col)
                 :unless (zerop q)
                 :do (progn
                       (loop :for c :from col :below (mcols m)
                          :for p* := (mref '(,type) m col c)
                          :for q* := (mref '(,type) m row c)
                          :for mindex := (+ (* mrf row) (* mcf c))
                          :do (setf (aref vals mindex) (+ (* p q*) (* p* q))))
                       (setf (aref vvals row) (+ (* p (aref vvals row))
                                                 (* (aref vvals col) q)))))))

          (back-substitute ()
            (let ((vvals (mvals v)))
              (declare (type (array ,type (*)) vvals))
              (loop :for col fixnum :from (1- (mrows m)) :downto 0
                 :for pivot ,type := (mref '(,type) m col col)
                 :unless (zerop pivot)
                 :do (setf (aref vvals col)
                           (/ (- (aref vvals col)
                                 (loop :for c :from (1+ col) :below (mcols m)
                                    :summing (the ,type
                                                  (* (mref '(,type) m col c)
                                                     (aref vvals c)))))
                              pivot))))))

     (loop :for col :below (mcols m)
        :for pivot-row := (find-pivot-row col)
        :unless (= pivot-row col)
        :do (progn
              (swap-rows-of-matrix pivot-row col)
              (swap-rows-of-vector pivot-row col))
        :do (eliminate-column col))
     (back-substitute)
     (values v m)))

(template:define-templated-function solve (type-m type-v) (m v)
  (let ((new-type-v (contagion-type type-m type-v)))
    `(policy-cond:with-expectations (> speed safety)
         ((assertion (= (mcols m) (mrows v)))
          (assertion (= (mrows m) (mcols m))))
       (let ((m (copy-matrix m ',new-type-v))
             (v (copy-matrix v ',new-type-v)))
         (solve-by-gaussian-elimination '(,new-type-v) m v)))))
