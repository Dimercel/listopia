(defpackage listopia-bench.accumulating
  (:use :cl
        :prove
        :listopia
        :listopia-bench.utils))
(in-package :listopia-bench.accumulating)


(plan nil)

(ok (bench ".map-accum-l"
           (.map-accum-l
            (lambda (acc x) (list acc (+ x acc)))
            1
            '(1 2 3))))

(ok (bench ".map-accum-r"
           (.map-accum-r
            (lambda (acc x) (list acc (+ x acc)))
            1
            '(1 2 3))))

(finalize)
