(defpackage listopia-bench.unfolding
  (:use :cl
        :prove
        :listopia
        :listopia-bench.utils))
(in-package :listopia-bench.unfolding)


(plan nil)

(ok (bench ".unfoldr" (.unfoldr
                       (lambda (x) (if (= x 0) nil (list x (1- x))))
                       5)))

(finalize)
