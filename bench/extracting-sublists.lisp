(defpackage listopia-bench.extracting-sublists
  (:use :cl
   :prove
        :listopia
   :listopia-bench.utils))
(in-package :listopia-bench.extracting-sublists)


(plan nil)

(ok (bench ".take" (.take 2 '(1 2 3))))
(ok (bench ".drop" (.drop 3 '(1 2 3 4 5))))
(ok (bench ".split-at" (.split-at 3 '(1 2 3 4 5))))
(ok (bench ".take-while" (.take-while #'evenp '(1 2 3 4))))
(ok (bench ".drop-while" (.drop-while #'numberp '(1 nil 3))))
(ok (bench ".drop-while-end" (.drop-while-end #'numberp '(nil nil 3))))
(ok (bench ".span" (.span #'evenp '(2 4 1 3))))
(ok (bench ".break" (.break #'evenp '(2 4 1 3))))
(ok (bench ".strip-prefix" (.strip-prefix '(1 2) '(1 2 3 4))))
(ok (bench ".inits" (.inits '(1 2 3))))
(ok (bench ".tails" (.tails '(1 2 3))))

(finalize)
