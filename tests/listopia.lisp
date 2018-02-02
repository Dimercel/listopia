(defpackage listopia-test
  (:use :cl
        :listopia
        :prove))

(in-package :listopia-test)


(plan nil)


;;; Basic functions


(is nil (.head '()))
(is 666 (.head '() 666))
(is 1 (.head '(1)))


(is nil (.last '()))
(is 666 (.last '() 666))
(is 1 (.last '(1)))


(is nil (.tail '()))
(is 666 (.tail '() 666))
(is nil (.tail '(1)))


(is nil (.init '()))
(is 666 (.init '() 666))
(is nil (.init '(1)))


(is nil (.uncons '()))
(is 666 (.uncons '() 666))
(is nil (.uncons '(1)))
(is 2 (length (.uncons '(1 2 3))))


;;; List transformations


(is '() (.map #'1+ '()))
(is '(2 3 4) (.map #'1+ '(1 2 3)))

(is '() (.intersperse 0 '()))
(is '(1) (.intersperse 0 '(1)))
(is 5 (length (.intersperse #\, '(#\f #\o \#o))))

(is '() (.intercalate '(0) '()))
(is '(1) (.intercalate '(0) '((1))))
(is 3 (length (.intercalate '(" ") '(("foo") ("bar")))))


;;; Reducing lists (folds)


(is '() (.foldl #'null '() '()))
(is '(1 2) (.foldl #'null '(1 2) '()))
(is 5 (length (.foldl (lambda (acc x)
                        (cons (1+ x) acc))
                      '(2 1)
                      '(2 3 4))))

(is-error (.foldl1 #'+ '()) 'simple-error)
(ok (numberp (.foldl1 #'+ '(1 2 3))))
(is (.foldl #'- 1 '(2 3)) (.foldl1 #'- '(1 2 3)))

(is '() (.foldr #'null '() '()))
(is '(1 2) (.foldr #'null '(1 2) '()))
(is 5 (length (.foldr (lambda (x acc)
                        (cons (1+ x) acc))
                      '(2 1)
                      '(4 3 2))))

(is-error (.foldr1 #'+ '()) 'simple-error)
(ok (numberp (.foldr1 #'+ '(1 2 3))))
(is (.foldr #'- 3 '(1 2)) (.foldr1 #'- '(1 2 3)))


;;; Special folds


(is 6 (length (.concat '((1) (2 3) (4 5 6)))))
(is '() (.concat '(() () ())))

(is 3 (length (.concat-map #'list '(1 2 3))))
(is '() (.concat-map #'list '()))

(ok (.and '(t t t)))
(ok (not (.and '(nil nil))))
(ok (.and '()))

(ok (not (.or '(nil nil))))
(ok (not (.or '())))

(ok (.any #'numberp '(1 2 3)))
(ok (.any #'numberp '("1" "2" 3)))
(ok (not (.any #'numberp '("1" "2"))))
(ok (not (.any #'numberp '())))

(ok (.all #'numberp '(1 2 3)))
(ok (not (.all #'numberp '(1 "2"))))
(ok (not (.all #'numberp '("1" "2"))))
(ok (.all #'numberp '()))

(is 0 (.sum '()))
(is 7 (.sum '(7)))

(is 1 (.product '()))
(is 7 (.product '(7)))

(is-error (.maximum '()) 'error)
(is 7 (.maximum '(7)))

(is-error (.minimum '()) 'error)
(is 7 (.minimum '(7)))


;;; Building lists


;;; Scans


(is '(1) (.scanl #'+ 1 '()))
(is 4 (length (.scanl #'+ 1 '(2 3 4))))

(is '() (.scanl1 #'+ '()))
(is 4 (length (.scanl1 #'+ '(1 2 3 4))))

(is '(1) (.scanr #'+ 1 '()))
(is 4 (length (.scanr #'+ 1 '(2 3 4))))

(is '() (.scanr1 #'+ '()))
(is 4 (length (.scanr1 #'+ '(1 2 3 4))))


;;; Accumulating maps


(is '(1 nil) (.map-accum-l #'null 1 '()))
(is 2 (length (.map-accum-l (lambda (acc x)
                              (list acc (+ acc x)))
                            1
                            '(2 3 4))))

(is '(1 nil) (.map-accum-r #'null 1 '()))
(is 2 (length (.map-accum-r (lambda (acc x)
                              (list acc (+ acc x)))
                            1
                            '(2 3 4))))


;; Infinite lists


(is '() (.iterate #'1+ 1 0))
(is 4 (length (.iterate #'1+ 1 4)))

(is '() (.repeat 1 0))
(is 4 (length (.repeat 1 4)))

(finalize)
