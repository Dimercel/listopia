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
           :.repeat
           :.replicate
           :.cycle

           ;; Unfolding
           :.unfoldr

           ;; Sublists
           ;; Extracting sublists
           :.take
           :.drop
           :.split-at
           :.take-while
           :.drop-while
           :.drop-while-end
           :.span
           :.break
           :.strip-prefix
           :.inits
           :.tails

           ;; Predicates
           :.is-prefix-of
           :.is-suffix-of
           :.is-infix-of
           :.is-subsequence-of

           ;; Searching lists
           ;; Searching by equality
           :.elem
           :.not-elem

           ;; Searching with a predicate
           :.find
           :.filter
           :.partition

           ;; Indexing lists
           :.elem-index
           :.elem-indices
           :.find-index
           :.find-indices

           ;; Zipping and unzipping lists
           .zip
           .zip-with
           .unzip
   ))

(in-package :listopia)


;;; Laziness


(defstruct lazy-list
  (part nil)
  (fn nil))


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
  (if (null list)
      default
      (rest list)))

(defun .init (list &optional (default nil))
  (if (rest list)
      (butlast list)
      default))

(defun .uncons (list &optional (default nil))
  (if (rest list)
      (list (first list)
            (rest list))
      default))


;;; List transformations


(defun .map (fn list)
  (map 'list fn list))

(defun .intersperse (sep list)
  (.init (.concat-map (lambda (x) (list x sep)) list)))

(defun .intercalate (sep list)
  (.concat (.intersperse sep list)))


;;; Reducing lists (folds)


(defun .foldl (fn init-val list)
  (reduce fn
          list
          :initial-value init-val))

(defun .foldl1 (fn list)
  (if (rest list)
      (.foldl fn (car list) (cdr list))
      (error "empty list")))

(defun .foldr (fn init-val list)
  (reduce fn
          list
          :from-end t
          :initial-value init-val))

(defun .foldr1 (fn list)
  (if (rest list)
      (.foldr fn (.last list) (.init list))
      (error "empty list")))


;;; Special folds


(defun .concat (list)
  (.foldl #'append nil list))

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
      (let ((acc init-val)
            (res nil))
        (dolist (item list)
          (setf acc (funcall fn acc item))
          (push acc res))
          (cons init-val (nreverse res)))))

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


(defun .iterate (fn init-val)
  (make-lazy-list :part (list init-val)
                  :fn (lambda () (.iterate fn (funcall fn init-val)))))

(defun .repeat (init-val)
  (make-lazy-list :part (list init-val)
                  :fn (lambda () (.repeat init-val))))

(defun .replicate (size init-val)
  (make-list size :initial-element init-val))

(defun .cycle (list)
  (when (null list)
    (error "empty list"))
  (make-lazy-list :part (copy-list list)
                  :fn (lambda () (.cycle list))))


;; Unfolding


(defun .unfoldr (fn init-val)
  (let ((iter-val (funcall fn init-val)))
    (when iter-val
      (cons (first iter-val) (.unfoldr fn (second iter-val))))))


;; Sublists


;; Extracting sublists


(defun .take (count list)
  (cond ((< count 0) (return-from .take nil))
        ((lazy-list-p list) (return-from .take (.take-lazy count list))))
  (if (<= count (length list))
      (subseq list 0 count)
      list))

(defun .take-lazy (count list)
  (let ((part (lazy-list-part list))
        (fn (lazy-list-fn list)))
    (if (>= (length part) count)
        (subseq part 0 count)
        (nconc part (.take-lazy (- count (length part)) (funcall fn))))))

(defun .drop (count list)
  (when (< count 0) (return-from .drop list))
  (nthcdr count list))

(defun .split-at (count list)
  (list (.take count list)
        (.drop count list)))

(defun .take-while (pred list)
  (loop for item in list
        while (funcall pred item)
        collect item))

(defun .drop-while (pred list)
  (unless (null list)
    (if (funcall pred (first list))
        (.drop-while pred (cdr list))
        list)))

(defun .drop-while-end (pred list)
  (.foldr (lambda (x acc)
            (if (and (funcall pred x) (null acc))
                '()
                (cons x acc)))
          '()
          list))

(defun .span (pred list)
  (list (.take-while pred list) (.drop-while pred list)))

(defun .break (pred list)
  (.span (lambda (x) (not (funcall pred x))) list))

(defun .strip-prefix (prefix list &optional (default nil))
  (if (equal (.take (length prefix) list) prefix)
      (.drop (length prefix) list)
      default))

(defun .inits (list)
  (let ((result nil))
    (dotimes (n (length list))
      (setf result (cons (subseq list 0 n) result)))
    (reverse (cons list result))))

(defun .tails (list)
  (let ((result nil))
    (dotimes (n (length list))
      (setf result (cons (subseq list n) result)))
    (reverse (cons nil result))))


;; Predicates


(defun .is-prefix-of (prefix list)
  (equal (.take (length prefix) list) prefix))

(defun .is-suffix-of (suffix list)
  (.is-prefix-of (reverse suffix) (reverse list)))

(defun .is-infix-of (infix list)
  (cond
    ((> (length infix) (length list)) nil)
    ((.is-prefix-of infix list) t)
    (t (.is-infix-of infix (cdr list)))))

(defun .is-subsequence-of (subseq list)
  (cond
    ((null subseq) t)
    ((null list) nil)
    (t (if (equal (car subseq) (car list))
           (.is-subsequence-of (cdr subseq) (cdr list))
           (.is-subsequence-of subseq (cdr list))))))


;; Searching lists


;; Searching by equality


(defun .elem (element list &key (test 'equalp))
  (not (null (member element list :test test))))

(defun .not-elem (element list &key (test 'equalp))
  (not (.elem element list :test test)))


;; Searching with a predicate


(defun .find (pred list &optional (default nil))
  (let ((tail (member-if pred list)))
    (if (null tail)
        default
        (car tail))))

(defun .filter (pred list)
  (remove-if-not pred list))

(defun .partition (pred list)
  (list (remove-if-not pred list)
        (remove-if pred list)))


;; Indexing lists


(defun .find-index (pred list &optional (default nil))
  (let ((inx (position-if pred list)))
    (if (null inx) default inx)))

(defun .elem-index (item list &optional (default nil))
  (.find-index (lambda (x) (equalp x item)) list default))

(defun .find-indices (pred list)
  (loop for i from 0 and e in list
        when (funcall pred e) collect i))

(defun .elem-indices (item list)
 (loop for i from 0 and e in list
        when (equal e item) collect i))


;; Zipping and unzipping lists


(defun .zip-with (fn &rest lists)
  (labels ((func (result fn lists)
             (if (some #'null lists)
                 (nreverse result)
                 (func
                  (push (apply fn (mapcar #'car lists)) result)
                  fn
                  (mapcar #'cdr lists)))))
    (func '() fn lists)))

(defun .zip (&rest lists)
  (apply #'.zip-with (cons #'list lists)))

(defun .unzip (lists)
  (apply #'.zip lists))
