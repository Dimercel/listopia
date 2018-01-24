#|
  This file is a part of listopia project.
  Copyright (c) 2018 Ito Dimercel (xolcman@gmail.com)
|#

(defsystem "listopia-test"
  :defsystem-depends-on ("prove-asdf")
  :author "Ito Dimercel"
  :license "LLGPL"
  :depends-on ("listopia"
               "prove")
  :components ((:module "tests"
                :components
                ((:test-file "listopia"))))
  :description "Test system for listopia"

  :perform (test-op (op c) (symbol-call :prove-asdf :run-test-system c)))
