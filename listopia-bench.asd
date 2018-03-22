#|
  This file is a part of listopia project.
  Copyright (c) 2018 Ito Dimercel (xolcman@gmail.com)
|#

(defsystem "listopia-bench"
  :defsystem-depends-on ("prove-asdf")
  :author "Ito Dimercel"
  :license "LLGPL"
  :depends-on ("listopia"
               "prove"
               "trivial-benchmark")
  :components ((:module "bench"
                :components
                ((:file "utils")
                 (:test-file "basic"))))
  :description "Benchmark system for listopia"

  :perform (test-op (op c) (symbol-call :prove-asdf :run-test-system c :reporter :dot)))
