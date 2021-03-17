(defpackage #:listopia/tests
  (:use #:cl
        #:listopia
        #:rove)
  (:shadowing-import-from #:listopia
                          #:and
                          #:break
                          #:find
                          #:last
                          #:map
                          #:or))

(in-package #:listopia/tests)


(deftest general-cases
  (testing "Basic functions"
    (ok (eql nil (head '())))
    (ok (eql 666 (head '() 666)))
    (ok (eql 1 (head '(1))))


    (ok (eql nil (last '())))
    (ok (eql 666 (last '() 666)))
    (ok (eql 1 (last '(1))))


    (ok (eql nil (tail '())))
    (ok (eql 666 (tail '() 666)))
    (ok (eql nil (tail '(1))))


    (ok (eql nil (init '())))
    (ok (eql 666 (init '() 666)))
    (ok (eql nil (init '(1))))


    (ok (eql nil (uncons '())))
    (ok (eql 666 (uncons '() 666)))
    (ok (eql nil (uncons '(1))))
    (ok (eql 2 (length (uncons '(1 2 3))))))

  (testing "List transformations"
    (ok (eql '() (map #'1+ '())))
    (ok (equal '(2 3 4) (map #'1+ '(1 2 3))))

    (ok (eql '() (intersperse 0 '())))
    (ok (equal '(1) (intersperse 0 '(1))))
    (ok (eql 5 (length (intersperse #\, '(#\f #\o \#o)))))

    (ok (eql '() (intercalate '(0) '())))
    (ok (eql '(1) (intercalate '(0) '((1)))))
    (ok (eql 3 (length (intercalate '(" ") '(("foo") ("bar")))))))


  (testing "Reducing lists(folds)"
    (ok (eql '() (foldl #'null '() '())))
    (ok (eql '(1 2) (foldl #'null '(1 2) '())))
    (ok (eql 5 (length (foldl (lambda (acc x)
                          (cons (1+ x) acc))
                        '(2 1)
                        '(2 3 4)))))

    (ok (signals (foldl1 #'+ '()) 'simple-error))
    (ok (numberp (foldl1 #'+ '(1 2 3))))
    (ok (eql (foldl #'- 1 '(2 3)) (foldl1 #'- '(1 2 3))))

    (ok (eql '() (foldr #'null '() '())))
    (ok (eql '(1 2) (foldr #'null '(1 2) '())))
    (ok (eql 5 (length (foldr (lambda (x acc)
                          (cons (1+ x) acc))
                        '(2 1)
                        '(4 3 2)))))

    (ok (signals (foldr1 #'+ '()) 'simple-error))
    (ok (numberp (foldr1 #'+ '(1 2 3))))
    (ok (eql (foldr #'- 3 '(1 2)) (foldr1 #'- '(1 2 3)))))


  (testing "Special folds"
    (ok (eql 6 (length (concat '((1) (2 3) (4 5 6))))))
    (ok (eql '() (concat '(() () ()))))

    (ok (eql 3 (length (concat-map #'list '(1 2 3)))))
    (ok (eql '() (concat-map #'list '())))

    (ok (and '(t t t)))
    (ok (not (and '(nil nil))))
    (ok (and '()))

    (ok (not (or '(nil nil))))
    (ok (not (or '())))

    (ok (any #'numberp '(1 2 3)))
    (ok (any #'numberp '("1" "2" 3)))
    (ok (not (any #'numberp '("1" "2"))))
    (ok (not (any #'numberp '())))

    (ok (all #'numberp '(1 2 3)))
    (ok (not (all #'numberp '(1 "2"))))
    (ok (not (all #'numberp '("1" "2"))))
    (ok (all #'numberp '()))

    (ok (eql 0 (sum '())))
    (ok (eql 7 (sum '(7))))

    (ok (eql 1 (product '())))
    (ok (eql 7 (product '(7))))

    (ok (signals (maximum '()) 'error))
    (ok (eql 7 (maximum '(7))))

    (ok (signals (minimum '()) 'error))
    (ok (eql 7 (minimum '(7)))))


  (testing "Scans"
    (ok (equal '(1) (scanl #'+ 1 '())))
    (ok (eql 4 (length (scanl #'+ 1 '(2 3 4)))))

    (ok (eql '() (scanl1 #'+ '())))
    (ok (eql 4 (length (scanl1 #'+ '(1 2 3 4)))))

    (ok (equal '(1) (scanr #'+ 1 '())))
    (ok (eql 4 (length (scanr #'+ 1 '(2 3 4)))))

    (ok (eql '() (scanr1 #'+ '())))
    (ok (eql 4 (length (scanr1 #'+ '(1 2 3 4))))))


  (testing "Accumulating maps"
    (ok (equal '(1 nil) (map-accum-l #'null 1 '())))
    (ok (eql 2 (length (map-accum-l (lambda (acc x)
                                (list acc (+ acc x)))
                              1
                              '(2 3 4)))))

    (ok (equal '(1 nil) (map-accum-r #'null 1 '())))
    (ok (eql 2 (length (map-accum-r (lambda (acc x)
                                (list acc (+ acc x)))
                              1
                              '(2 3 4))))))


  (testing "Infinite lists"
    (ok (eql 4 (length (take 4 (iterate #'1+ 1)))))

    (ok (eql 4 (length (take 4 (repeat 1)))))

    (ok (equal '() (replicate 0 1)))
    (ok (eql 4 (length (replicate 4 1))))

    (ok (signals (cycle '()) 'error))
    (ok (eql 5 (length (take 5 (cycle '(1 2 3)))))))


  (testing "Unfolding"
    (ok (equal (take 4 (iterate #'1+ 0)) (unfoldr (lambda (x)
                                            (if (< x 4)
                                                (list x (1+ x))))
                                          0)))
    (ok (equal '() (unfoldr (lambda (x) (declare (ignore x))
                      nil) 0))))



  (testing "Extracting sublists"
    (ok (equal '() (take 0 '(1 2 3))))
    (ok (equal '() (take -1 '(1 2 3))))
    (ok (eql 4 (length (take 10 '(1 2 3 4)))))
    (ok (eql 4 (length (take 4 (cycle '(1 2))))))
    (ok (eql 4 (length (take 4 (iterate #'identity nil)))))

    (ok (equal '() (drop 3 '(1 2))))
    (ok (equal '() (drop 3 '())))
    (ok (equal '(1 2) (drop -1 '(1 2))))
    (ok (equal '(1 2) (drop 0 '(1 2))))

    (ok (eql 2 (length (split-at 3 '(1 2 3 4 5)))))
    (ok (equal (split-at 3 '(1 2 3)) (split-at 5 '(1 2 3))))
    (ok (equal (split-at 0 '(1 2 3)) (split-at -1 '(1 2 3))))

    (ok (eql 2 (length (take-while #'null '(nil nil 1 2 nil)))))
    (ok (equal '() (take-while #'null '())))

    (ok (eql 2 (length (drop-while #'numberp '(1 2 3 "foo" "bar")))))
    (ok (equal '() (drop-while #'numberp '(1 2 3))))

    (ok (eql 2 (length (drop-while-end #'numberp '("foo" "bar" 1 2 3)))))
    (ok (equal '() (drop-while-end #'numberp '(1 2 3))))

    (ok (eql 2 (length (span #'numberp '(1 2 "three" "four")))))
    (ok (equal '(nil nil) (span #'numberp '())))
    (ok (equal (list (take-while #'numberp '(1 2 "three" "four"))
              (drop-while #'numberp '(1 2 "three" "four")))
        (span #'numberp '(1 2 "three" "four"))))

    (ok (eql 2 (length (break #'numberp '(1 2 "three" "four")))))
    (ok (equal '(nil nil) (break #'numberp '())))

    (ok (eql 3 (length (strip-prefix '(1 2) '(1 2 3 4 5)))))
    (ok (eql 42 (strip-prefix '(1 2) '() 42)))
    (ok (eql 3 (length (strip-prefix '() '(1 2 3)))))

    (ok (eql 4 (length (inits '(1 2 3)))))
    (ok (equal '(nil) (inits '())))
    (ok (null (first (inits '(1 2 3)))))

    (ok (eql 4 (length (tails '(1 2 3)))))
    (ok (equal '(nil) (tails '())))
    (ok (null (last (tails '(1 2 3))))))


  (testing "Predicates"
    (ok (is-prefix-of '(1 2) '(1 2 3)))
    (ok (is-prefix-of '() '(1 2 3)))
    (ok (not (is-prefix-of '(1 2) '())))
    (ok (is-prefix-of '() '()))

    (ok (is-suffix-of '(2 1) '(3 2 1)))
    (ok (is-suffix-of '() '(1 2 3)))
    (ok (not (is-suffix-of '(1 2) '())))
    (ok (is-suffix-of '() '()))

    (ok (is-infix-of '(1 2) '(3 4 1 2 3 4)))
    (ok (is-infix-of '() '(1 2 3)))
    (ok (not (is-infix-of '(1 2) '())))
    (ok (is-infix-of '() '()))

    (ok (is-subsequence-of '(1 2 3) '(1 0 2 0 3 0)))
    (ok (is-subsequence-of '() '(1 2 3)))
    (ok (not (is-subsequence-of '(1 2 3) '()))))


  (testing "Searching by equality"
    (ok (elem 1 '(1 2 3)))
    (ok (not (elem 1 '())))

    (ok (not-elem :foo '(:bar 1 2)))
    (ok (not (not-elem 1 '(1 2 3)))))


  (testing "Searching with a predicate"
    (ok (eql 666 (find #'numberp '(:foo :bar) 666)))
    (ok (eql nil (find #'numberp '(:foo bar))))

    (ok (eql 3 (length (filter #'numberp '(:foo 1 :bar 2 3)))))
    (ok (null (filter #'numberp '(:foo :bar))))
    (ok (null (filter #'numberp '())))

    (ok (eql 2 (length (partition #'numberp '(:foo 1 :bar 2 3)))))
    (ok (null (first (partition #'numberp '(:foo :bar)))))
    (ok (null (second (partition #'keywordp '(:foo :bar))))))


  (testing "Indexing lists"
    (ok (eql nil (elem-index 0 '(1 2 3))))
    (ok (eql 42 (elem-index 0 '(1 2 3) 42)))

    (ok (eql nil (find-index #'keywordp '(1 2 3))))
    (ok (eql 42 (find-index #'keywordp '(1 2 3) 42)))

    (ok (eql 0 (length (find-indices #'keywordp '(1 2 3)))))
    (ok (not (= 0 (length (find-indices #'keywordp '(1 :foo 3 :bar))))))
    (ok (equal '(0 2) (find-indices #'null '(nil 1 nil 2 3))))

    (ok (eql 0 (length (elem-indices 42 '(1 2 3)))))
    (ok (not (= 0 (length (elem-indices 42 '(1 42 3 42))))))
    (ok (equal '(1 3) (elem-indices 7 '(0 7 2 7 4 5)))))


  (testing "Zipping and unzipping lists"
    (ok (eql 2 (length (zip '(1 2) '(3 4) '(5 6)))))
    (ok (equal '() (zip '(1 2) '() '(3 4))))
    (ok (equal '() (zip '() '())))

    (ok (eql 2 (length (zip-with #'+ '(1 2) '(3 4) '(5 6)))))
    (ok (equal '() (zip-with #'+ '(1 2) '() '(3 4))))
    (ok (equal '() (zip-with #'+ '() '())))

    (ok (eql 2 (length (unzip '((1 2) '(3 4) '(5 6))))))
    (ok (eql 2 (length (unzip '((1 2) '(3 4) '(5 6 7))))))
    (ok (equal '() (unzip '((1 2) () (3 4)))))
    (ok (equal '() (unzip '(() ()))))))


