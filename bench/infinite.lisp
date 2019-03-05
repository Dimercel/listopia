(defpackage listopia-bench.infinite
  (:use :cl
        :prove
        :listopia
        :listopia-bench.utils))
(in-package :listopia-bench.infinite)


(plan nil)

(ok (bench ".iterate" (.iterate #'1+ 0)))
(ok (bench ".repeat" (.repeat 1 4)))
(ok (bench ".replicate" (.replicate 4 1)))
(ok (bench ".cycle" (.cycle '(1 2 3) 5)))

(finalize)
