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

           ;; Reducing lists (folds)
           :.foldl
           :.foldl1
           :.foldr
           :.foldr1

           ;; Special folds
           :.concat
           :.concat-map
           :.and
           :.or
           :.any
           :.all
           :.sum
           :.product
           :.maximum
           :.minimum

           ;; Building lists
           ;; Scans
           :.scanl
           :.scanl1
           :.scanr
           :.scanr1

           ;; Accumulating maps
           :.map-accum-l
           :.map-accum-r

           ;; Infinite lists
           :.iterate
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


;;; Reducing lists (folds)


(defun .foldl (fn init-val list)
  (reduce fn
          list
          :initial-value init-val))

(defun .foldl1 (fn list)
  (if (> (length list) 1)
      (.foldl fn (car list) (cdr list))
      (error "empty list")))

(defun .foldr (fn init-val list)
  (reduce fn
          list
          :from-end t
          :initial-value init-val))

(defun .foldr1 (fn list)
  (if (> (length list) 1)
      (.foldr fn (.last list) (.init list))
      (error "empty list")))


;;; Special folds


(defun .concat (list)
  (apply #'concatenate
         (cons 'list list)))

(defun .concat-map (fn list)
  (.concat (.map fn list)))

(defun .and (list)
  (notany #'null list))

(defun .or (list)
  (notevery #'null list))

(defun .any (fn list)
  (some fn list))

(defun .all (fn list)
  (every fn list))

(defun .sum (list)
  (apply #'+ list))

(defun .product (list)
  (apply #'* list))

(defun .maximum (list)
  (apply #'max list))

(defun .minimum (list)
  (apply #'min list))


;;; Building lists


;;; Scans


(defun .scanl (fn init-val list)
  (if (null list)
      (list init-val)
      (cons init-val
            (.scanl fn
                    (funcall fn init-val (car list))
                    (cdr list)))))

(defun .scanl1 (fn list)
  (unless (null list)
    (.scanl fn (car list) (cdr list))))

(defun .scanr (fn init-val list)
  (if (null list)
      (list init-val)
      (append
       (.scanr fn
               (funcall fn init-val (.last list))
               (.init list))
       (list init-val))))

(defun .scanr1 (fn list)
  (unless (null list)
    (.scanr fn (.last list) (.init list))))


;; Accumulating maps


(defun .map-accum-l (fn init-val list)
  (.foldl
   (lambda (acc x)
     (let ((iter (funcall fn (first acc) x)))
       (list (first iter)
             (append (second acc)
                     (list (second iter))))))
   (list init-val nil)
   list))

(defun .map-accum-r (fn init-val list)
  (.foldr
   (lambda (x acc)
     (let ((iter (funcall fn (first acc) x)))
       (list (first iter)
             (cons (second iter)
                   (second acc)))))
   (list init-val nil)
   list))


;; Infinite lists


(defun .iterate (fn init-val size)
  (when (> size 0)
    (cons init-val (.iterate fn (funcall fn init-val) (1- size)))))
