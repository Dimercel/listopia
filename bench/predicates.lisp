(defpackage listopia-bench.predicates
  (:use :cl
        :prove
        :listopia
        :listopia-bench.utils))
(in-package :listopia-bench.predicates)


(plan nil)


(ok (bench ".is-prefix-of"
           (.is-prefix-of '(1 2 3) '(1 2 3 4 5))))

(ok (bench ".is-suffix-of"
           (.is-suffix-of '(3 2 1) '(5 4 3 2 1))))

(ok (bench ".is-infix-of"
           (.is-suffix-of '(1 2) '(3 3 1 2 3 3))))


(finalize)
