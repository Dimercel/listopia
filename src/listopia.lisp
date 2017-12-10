(defpackage :listopia
  (:use :cl)
  (:export :.head
           :.last
           :.tail
           :.init
           :.uncons))

(in-package :listopia)


(defun .head (list &optional (default nil))
  (if (null list)
      default
      (car list)))

(defun .last (list &optional (default nil))
  (if (null list)
      default
      (car (last list))))

(defun .tail (list &optional (default nil))
  (if (> (length list) 1)
      (cdr list)
      default))

(defun .init (list &optional (default nil))
  (if (> (length list) 1)
      (butlast list)
      default))

(defun .uncons (list &optional (default nil))
  (if (> (length list) 1)
      (list (car list)
            (cdr list))
      default))
