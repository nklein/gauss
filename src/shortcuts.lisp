;;; src/shortcuts.lisp

(in-package #:gauss)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun just-the-arguments (lambda-list)
    (labels ((first-atom (atom-or-list)
               (cond
                 ((null atom-or-list) nil)
                 ((listp atom-or-list) (first-atom (first atom-or-list)))
                 (t atom-or-list))))
      (mapcar #'first-atom
              (set-difference lambda-list
                              lambda-list-keywords))))

  (defun make-matrix-operation-shortcuts (extension-a type-a
                                          extension-b type-b)
    (let ((pkg (if (symbolp extension-a)
                   (symbol-package extension-a)
                   *package*)))

      (labels ((shortcut-name (function-symbol &rest extensions)
                 (intern (format nil
                                 "~A/~A"
                                 function-symbol
                                 (apply #'concatenate
                                        'string
                                        (mapcar #'string extensions)))
                         pkg))

               (make-shortcut (function-symbol
                               lambda-list
                               shortcut-name
                               &rest types)
                 `((declaim (inline ,shortcut-name))
                   (defun ,shortcut-name (&rest args)
                     (apply #',function-symbol '(,@types) args))
                   (define-compiler-macro ,shortcut-name (&whole form
                                                                 ,@lambda-list)
                     (declare (ignore ,@(just-the-arguments lambda-list)))
                     (list* ',function-symbol
                            (list 'quote '(,@types))
                            (rest form)))))

               (make-shortcut-1 (function-symbol lambda-list)
                 (make-shortcut function-symbol
                                lambda-list
                                (shortcut-name function-symbol extension-a)
                                type-a))
               (make-shortcut-2 (function-symbol lambda-list)
                 (append (make-shortcut function-symbol
                                        lambda-list
                                        (shortcut-name function-symbol
                                                       extension-a
                                                       extension-b)
                                        type-a
                                        type-b)
                         (unless (eql type-a type-b)
                           (make-shortcut function-symbol
                                          lambda-list
                                          (shortcut-name function-symbol
                                                         extension-b
                                                         extension-a)
                                          type-b
                                          type-a)))))

        (append '(progn)
                (make-shortcut-1 'make-matrix '(rows cols values))
                (make-shortcut-1 'make-matrix* '(rows cols &rest values))
                (make-shortcut-1 'make-vector '(values))
                (make-shortcut-1 'make-vector* '(&rest values))
                (make-shortcut-1 'mref '(row col))
                (make-shortcut-1 'vref '(row))
                (make-shortcut-1 'transpose '(m))
                (make-shortcut-2 'm+ '(a b))
                (make-shortcut-2 'v+ '(a b))
                (make-shortcut-2 'm- '(a b))
                (make-shortcut-2 'v- '(a b))
                (make-shortcut-2 'scale '(a b))
                (make-shortcut-2 'm* '(a b))
                (make-shortcut-2 'solve '(a b))
                `(',(shortcut-name 'm+ extension-a extension-b)))))))

(defmacro define-matrix-operation-shortcuts (extension-a type-a
                                             extension-b type-b)
  (make-matrix-operation-shortcuts extension-a type-a
                                   extension-b type-b))

(define-matrix-operation-shortcuts q rational q rational)
(define-matrix-operation-shortcuts s single-float s single-float)
(define-matrix-operation-shortcuts d double-float d double-float)
