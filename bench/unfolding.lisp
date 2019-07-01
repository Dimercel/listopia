(defpackage listopia-bench.unfolding
  (:use :cl
        :prove
        :listopia-bench.utils)
  (:import-from :listopia
                :unfoldr))

(in-package :listopia-bench.unfolding)


(plan nil)

(ok (bench "unfoldr" (unfoldr
                       (lambda (x) (if (= x 0) nil (list x (1- x))))
                       5)))

(finalize)
