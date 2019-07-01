(defpackage listopia-bench.accumulating
  (:use :cl
        :prove
        :listopia-bench.utils)
  (:import-from :listopia
                :map-accum-l
                :map-accum-r))

(in-package :listopia-bench.accumulating)


(plan nil)

(ok (bench "map-accum-l"
           (map-accum-l
            (lambda (acc x) (list acc (+ x acc)))
            1
            '(1 2 3))))

(ok (bench "map-accum-r"
           (map-accum-r
            (lambda (acc x) (list acc (+ x acc)))
            1
            '(1 2 3))))

(finalize)
