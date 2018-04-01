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
                 (:test-file "basic")
                 (:test-file "transformations")
                 (:test-file "reducing-folds")
                 (:test-file "special-folds")
                 (:test-file "scans")
                 (:test-file "accumulating")
                 (:test-file "infinite")
                 (:test-file "unfolding")
                 )))
  :description "Benchmark system for listopia"

  :perform (test-op (op c) (symbol-call :prove-asdf :run-test-system c :reporter :dot)))
