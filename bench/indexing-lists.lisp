(defpackage listopia-bench.indexing-lists
  (:use :cl
        :prove
        :listopia-bench.utils)
  (:import-from :listopia
                :elem-index
                :find-index
                :find-indices
                :elem-indices))
(in-package :listopia-bench.indexing-lists)


(plan nil)

(ok (bench "elem-index" (elem-index 2 '(1 2 3))))
(ok (bench "find-index" (find-index #'keywordp '(1 :foo 3))))
(ok (bench "find-indices" (find-indices #'keywordp '(1 :foo 3 :bar))))
(ok (bench "elem-indices" (elem-indices 42 '(1 42 3 42))))

(finalize)
