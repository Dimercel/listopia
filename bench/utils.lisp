(defpackage listopia-bench.utils
  (:use :cl)
  (:export :bench))
(in-package :listopia-bench.utils)


(defmacro bench (title &body forms)
  `(progn
     (format t "~%Benchmarking: ~S" ,title)
     (time
      (benchmark:with-timing (1000)
        ,@forms))
     t))
