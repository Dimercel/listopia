(defpackage listopia-bench.searching-lists
  (:use :cl
        :prove
        :listopia-bench.utils)
  (:import-from :listopia
                :elem
                :not-elem
                :filter
                :partition)
  (:shadowing-import-from :listopia :find))

(in-package :listopia-bench.searching-lists)


(plan nil)


(ok (bench "elem"
           (elem 1 '(1 2 3))))

(ok (bench "not-elem"
           (not-elem 1 '(2 3 4))))

(ok (bench "find"
           (find #'numberp '(:foo :bar 1))))

(ok (bench "filter"
           (filter #'numberp '(:foo 1 :bar 2))))

(ok (bench "partition"
           (partition #'numberp '(:foo 1 :bar 2))))

(finalize)
