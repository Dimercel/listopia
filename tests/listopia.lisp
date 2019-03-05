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


(is 4 (length (.take 4 (.iterate #'1+ 1))))

(is 4 (length (.take 4 (.repeat 1))))

(is '() (.replicate 0 1))
(is 4 (length (.replicate 4 1)))

(is-error (.cycle '()) 'error)
(is 5 (length (.take 5 (.cycle '(1 2 3)))))


;; Unfolding


(is (.take 4 (.iterate #'1+ 0)) (.unfoldr (lambda (x)
                                    (if (< x 4)
                                        (list x (1+ x))))
                                  0))
(is '() (.unfoldr (lambda (x) (declare (ignore x))
                    nil) 0))


;; Sublists


;; Extracting sublists


(is '() (.take 0 '(1 2 3)))
(is '() (.take -1 '(1 2 3)))
(is 4 (length (.take 10 '(1 2 3 4))))
(is 4 (length (.take 4 (.cycle '(1 2)))))
(is 4 (length (.take 4 (.iterate #'identity nil))))

(is '() (.drop 3 '(1 2)))
(is '() (.drop 3 '()))
(is '(1 2) (.drop -1 '(1 2)))
(is '(1 2) (.drop 0 '(1 2)))

(is 2 (length (.split-at 3 '(1 2 3 4 5))))
(is (.split-at 3 '(1 2 3)) (.split-at 5 '(1 2 3)))
(is (.split-at 0 '(1 2 3)) (.split-at -1 '(1 2 3)))

(is 2 (length (.take-while #'null '(nil nil 1 2 nil))))
(is '() (.take-while #'null '()))

(is 2 (length (.drop-while #'numberp '(1 2 3 "foo" "bar"))))
(is '() (.drop-while #'numberp '(1 2 3)))

(is 2 (length (.drop-while-end #'numberp '("foo" "bar" 1 2 3))))
(is '() (.drop-while-end #'numberp '(1 2 3)))

(is 2 (length (.span #'numberp '(1 2 "three" "four"))))
(is '(nil nil) (.span #'numberp '()))
(is (list (.take-while #'numberp '(1 2 "three" "four"))
          (.drop-while #'numberp '(1 2 "three" "four")))
    (.span #'numberp '(1 2 "three" "four")))

(is 2 (length (.break #'numberp '(1 2 "three" "four"))))
(is '(nil nil) (.break #'numberp '()))

(is 3 (length (.strip-prefix '(1 2) '(1 2 3 4 5))))
(is 42 (.strip-prefix '(1 2) '() 42))
(is 3 (length (.strip-prefix '() '(1 2 3))))

(is 4 (length (.inits '(1 2 3))))
(is '(nil) (.inits '()))
(ok (null (first (.inits '(1 2 3)))))

(is 4 (length (.tails '(1 2 3))))
(is '(nil) (.tails '()))
(ok (null (.last (.tails '(1 2 3)))))


;; Predicates


(ok (.is-prefix-of '(1 2) '(1 2 3)))
(ok (.is-prefix-of '() '(1 2 3)))
(ok (not (.is-prefix-of '(1 2) '())))
(ok (.is-prefix-of '() '()))

(ok (.is-suffix-of '(2 1) '(3 2 1)))
(ok (.is-suffix-of '() '(1 2 3)))
(ok (not (.is-suffix-of '(1 2) '())))
(ok (.is-suffix-of '() '()))

(ok (.is-infix-of '(1 2) '(3 4 1 2 3 4)))
(ok (.is-infix-of '() '(1 2 3)))
(ok (not (.is-infix-of '(1 2) '())))
(ok (.is-infix-of '() '()))

(ok (.is-subsequence-of '(1 2 3) '(1 0 2 0 3 0)))
(ok (.is-subsequence-of '() '(1 2 3)))
(ok (not (.is-subsequence-of '(1 2 3) '())))


;; Searching lists
;; Searching by equality


(ok (.elem 1 '(1 2 3)))
(ok (not (.elem 1 '())))

(ok (.not-elem :foo '(:bar 1 2)))
(ok (not (.not-elem 1 '(1 2 3))))


;; Searching with a predicate


(is 666 (.find #'numberp '(:foo :bar) 666))
(is nil (.find #'numberp '(:foo bar)))

(is 3 (length (.filter #'numberp '(:foo 1 :bar 2 3))))
(ok (null (.filter #'numberp '(:foo :bar))))
(ok (null (.filter #'numberp '())))

(is 2 (length (.partition #'numberp '(:foo 1 :bar 2 3))))
(ok (null (first (.partition #'numberp '(:foo :bar)))))
(ok (null (second (.partition #'keywordp '(:foo :bar)))))


;; Indexing lists


(is nil (.elem-index 0 '(1 2 3)))
(is 42 (.elem-index 0 '(1 2 3) 42))

(is nil (.find-index #'keywordp '(1 2 3)))
(is 42 (.find-index #'keywordp '(1 2 3) 42))

(is 0 (length (.find-indices #'keywordp '(1 2 3))))
(ok (not (= 0 (length (.find-indices #'keywordp '(1 :foo 3 :bar))))))

(is 0 (length (.elem-indices 42 '(1 2 3))))
(ok (not (= 0 (length (.elem-indices 42 '(1 42 3 42))))))


;; Zipping and unzipping lists


(is 2 (length (.zip '(1 2) '(3 4) '(5 6))))
(is '() (.zip '(1 2) '() '(3 4)))
(is '() (.zip '() '()))

(is 2 (length (.zip-with #'+ '(1 2) '(3 4) '(5 6))))
(is '() (.zip-with #'+ '(1 2) '() '(3 4)))
(is '() (.zip-with #'+ '() '()))

(is 2 (length (.unzip '((1 2) '(3 4) '(5 6)))))
(is 2 (length (.unzip '((1 2) '(3 4) '(5 6 7)))))
(is '() (.unzip '((1 2) () (3 4))))
(is '() (.unzip '(() ())))


(finalize)
