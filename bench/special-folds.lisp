(defpackage listopia-bench.special-folds
  (:use :cl
        :prove
        :listopia-bench.utils)
  (:import-from :listopia
                :concat
                :concat-map
                :any
                :all
                :sum
                :product
                :maximum
                :minimum)
  (:shadowing-import-from :listopia :and :or))

(in-package :listopia-bench.special-folds)


(plan nil)

(ok (bench "concat" (concat '((1) (1 2) (1 2 3)))))
(ok (bench "concat-map" (concat-map #'list '(1 2 3))))
(ok (bench "and" (and '(t nil t))))
(ok (bench "or" (or '(t nil t))))
(ok (bench "any" (any #'numberp '("1" "2" 3))))
(ok (bench "all" (all #'numberp '(1 2 3))))
(ok (bench "sum" (sum '(1 2 3))))
(ok (bench "product" (product '(1 2 3))))
(ok (bench "maximum" (maximum '(1 2 3))))
(ok (bench "minimum" (minimum '(1 2 3))))

(finalize)
