(defpackage listopia-bench.basic
  (:use :cl
        :prove
        :listopia
        :listopia-bench.utils))
(in-package :listopia-bench.basic)


(plan nil)

(ok (bench ".head" (.head '(1 2 3))))
(ok (bench ".last" (.last '(1 2 3))))
(ok (bench ".tail" (.tail '(1 2 3))))
(ok (bench ".init" (.init '(1 2 3))))
(ok (bench ".uncons" (.uncons '(1 2 3))))

(finalize)
