(defpackage listopia-bench.transformations
  (:use :cl
        :prove
        :listopia-bench.utils)
  (:import-from :listopia
                :intersperse
                :intercalate)
  (:shadowing-import-from :listopia :map))

(in-package :listopia-bench.transformations)


(plan nil)

(ok (bench "map" (map #'1+ '(1 2 3))))
(ok (bench "intersperse" (intersperse 0 '(1 2 3))))
(ok (bench "intercalate" (intercalate '(0) '((1) (2) (3)))))

(finalize)
