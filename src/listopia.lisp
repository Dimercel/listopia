(defpackage :listopia
  (:use :cl)
  (:export :.head
           :.last
           :.tail
           :.init
           :.uncons

           ;; List transformations
           :.map
           :.intersperse
           :.intercalate

           ;; Special folds
           :.concat
           ))

(in-package :listopia)


;;; Basic functions


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


;;; List transformations


(defun .map (fn list)
  (map 'list fn list))

(defun .intersperse (sep list)
  (labels ((internal-fn (sep list)
             (if (null list)
                 nil
                 (append (list (car list) sep)
                         (internal-fn sep (cdr list))))))
    (.init (internal-fn sep list) nil)))

(defun .intercalate (sep list)
  (.concat (.intersperse sep list)))


;;; Special folds


(defun .concat (list)
  (apply #'concatenate
         (cons 'list list)))
