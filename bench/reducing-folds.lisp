(defpackage listopia-bench.reducing-folds
  (:use :cl
        :prove
        :listopia
        :listopia-bench.utils))
(in-package :listopia-bench.reducing-folds)


(plan nil)

(ok (bench ".foldl" (.foldl (lambda (acc x) (cons (1+ x) acc)) nil '(1 2 3))))
(ok (bench ".foldl1" (.foldl1 (lambda (acc x) (cons (1+ x) acc)) '(1 2 3))))
(ok (bench ".foldr" (.foldr (lambda (x acc) (cons (1+ x) acc)) nil '(1 2 3))))
(ok (bench ".foldr1" (.foldr1 (lambda (x acc) (cons (1+ x) acc)) '(1 2 3))))

(finalize)
