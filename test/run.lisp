;;;; test/run.lisp

(in-package #:gauss-test)

(defun run-all-tests (&key (debug-on-error nst:*debug-on-error*)
                           (debug-on-fail nst:*debug-on-fail*))
  (let ((nst:*debug-on-error* debug-on-error)
        (nst:*debug-on-fail* debug-on-fail))
    (nst:nst-cmd :run-package #.*package*)))
