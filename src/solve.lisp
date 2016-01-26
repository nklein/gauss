;;; src/solve.lisp

(in-package #:gauss)

(template:define-templated-function solve-by-gaussian-elimination
    (type-m type-v) (m v)
  `(flet ((find-pivot-row (col)
            (loop :with best-pivot-row fixnum := col
               :with best-pivot-val ,type-m := (mref '(,type-m) m col col)
               :for row fixnum :from (1+ best-pivot-row) :below (mrows m)
               :for val ,type-m := (mref '(,type-m) m row col)
               :when (< best-pivot-val val)
                 :do (setf best-pivot-row row
                           best-pivot-val val)
               :finally (return best-pivot-row)))

          (swap-rows-of-matrix (row-a row-b)
            (let ((vals (mvals m))
                  (mrf (mrf m))
                  (mcf (mcf m)))
              (declare (type (array ,type-m) vals)
                       (type fixnum mrf mcf))
              (loop :with mrf-a fixnum := (* mrf row-a)
                    :with mrf-b fixnum := (* mcf row-b)
                 :for col :below (mcols m)
                 :for a := (+ mrf-a (* mcf col))
                 :for b := (+ mrf-b (* mcf col))
                 :do (rotatef (aref vals a) (aref vals b)))))

          (swap-rows-of-vector (row-a row-b)
            (let ((vals (mvals v)))
              (declare (type (array ,type-v) vals))
              (rotatef (aref vals row-a) (aref vals row-b))))

          (eliminate-column (col)
            (declare (ignore col))
            (error "Not implemented yet"))

          (back-substitute ()
            (error "Not implemented yet")))

     (loop :for col :below (mcols m)
        :for pivot-row := (find-pivot-row col)
        :unless (= pivot-row col)
          :do (progn
                (swap-rows-of-matrix pivot-row col)
                (swap-rows-of-vector pivot-row col))
        :do (eliminate-column col))
     (back-substitute)
     v))

(template:define-templated-function solve (type-m type-v) (m v)
  `(policy-cond:with-expectations (> speed safety)
       ((assertion (= (mcols m) (mrows v)))
        (assertion (= (mrows m) (mcols m))))
     (solve-by-gaussian-elimination '(,type-m ,type-v)
                                    (copy-matrix m)
                                    (copy-matrix v))))
