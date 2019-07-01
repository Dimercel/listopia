(defpackage listopia-bench.infinite
  (:use :cl
        :prove
        :listopia-bench.utils)
  (:import-from :listopia
                :iterate
                :repeat
                :replicate
                :cycle))

(in-package :listopia-bench.infinite)


(plan nil)

(ok (bench "iterate" (iterate #'1+ 0)))
(ok (bench "repeat" (repeat 1)))
(ok (bench "replicate" (replicate 4 1)))
(ok (bench "cycle" (cycle '(1 2 3))))

(finalize)
