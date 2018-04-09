(defpackage listopia-bench.predicates
  (:use :cl
        :prove
        :listopia
        :listopia-bench.utils))
(in-package :listopia-bench.predicates)


(plan nil)


(ok (bench ".is-prefix-of"
           (.is-prefix-of '(1 2 3) '(1 2 3 4 5))))


(finalize)
