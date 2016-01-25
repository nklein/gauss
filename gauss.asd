;;;; gauss.asd

(asdf:defsystem #:gauss
  :description "Yet another matrix library."
  :author "Patrick Stein <pat@nklein.com>"
  :version "0.1.20160124"
  :license "UNLICENSE"
  :depends-on (#:policy-cond #:template)
  :in-order-to ((asdf:test-op (asdf:load-op :gauss-test)))
  :perform (asdf:test-op (o c)
             (uiop:symbol-call :gauss-test :run-all-tests))
  :components
  ((:static-file "README.md")
   (:module "src"
    :components ((:file "package")
                 (:file "construct" :depends-on ("package"))
                 (:file "mref" :depends-on ("package"))
                 (:file "add" :depends-on ("package"
                                           "construct"
                                           "mref"))
                 (:file "transpose" :depends-on ("package"
                                                 "construct"))
                 (:file "declare" :depends-on ("package"
                                               "construct"
                                               "mref"
                                               "add"
                                               "transpose"))))))

(asdf:defsystem #:gauss-test
  :description "Tests for the GAUSS package."
  :author "Patrick Stein <pat@nklein.com>"
  :version "0.1.20160124"
  :license "UNLICENSE"
  :depends-on ((:version #:gauss "0.1.20160124")
               #:nst)
  :components
  ((:module "test"
    :components ((:file "package")
                 (:file "construct" :depends-on ("package"))
                 (:file "add" :depends-on ("package"))
                 (:file "transpose" :depends-on ("package"))
                 (:file "run" :depends-on ("package"))))))
