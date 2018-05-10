(defpackage listopia-bench.indexing-lists
  (:use :cl
        :prove
        :listopia
        :listopia-bench.utils))
(in-package :listopia-bench.indexing-lists)


(plan nil)

(ok (bench ".elem-index" (.elem-index 2 '(1 2 3))))

(finalize)
