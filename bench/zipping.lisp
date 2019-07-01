(defpackage listopia-bench.zipping
  (:use :cl
        :prove
        :listopia-bench.utils)
  (:import-from :listopia
                :zip
                :zip-with
                :unzip))

(in-package :listopia-bench.zipping)


(plan nil)


(ok (bench "zip" (zip '(1 2 3) '(4 5 6) '(7 8 9))))
(ok (bench "zip-with" (zip-with #'list '(1 2 3) '(4 5 6) '(7 8 9))))
(ok (bench "unzip" (unzip '((1 2 3) (4 5 6) (7 8 9)))))


(finalize)
