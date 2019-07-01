(defpackage listopia-bench.scans
  (:use :cl
        :prove
        :listopia-bench.utils)
  (:import-from :listopia
                :scanl
                :scanl1
                :scanr
                :scanr1))

(in-package :listopia-bench.scans)


(plan nil)

(ok (bench "scanl" (scanl #'+ 1 '(1 2 3))))
(ok (bench "scanl1" (scanl1 #'+ '(1 2 3))))
(ok (bench "scanr" (scanr #'+ 1 '(1 2 3))))
(ok (bench "scanr1" (scanr1 #'+ '(1 2 3))))

(finalize)
