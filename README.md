# Listopia

 This is no official port of Haskell package [Data.List](https://hackage.haskell.org/package/base-4.10.1.0/docs/Data-List.html)

## Usage

```common-lisp
(:use :listopia)
```

## Installation

```common-lisp
(ql:quickload :listopia)
```


## Functions

All functions and constructs in the library are prefixed with a point (.)

### Basic functions

#### .head `(list &optional (default nil))`

Extract the first element of a list. If list is empty, returns default value.

```common-lisp
(.head '(1 2 3)) ;; => 1
(.head '(1)) ;; => 1
(.head '() 666) ;; => 666
```

#### .last `(list &optional (default nil))`

Extract the last element of a list. If list is empty, returns default value.

```common-lisp
(.last '(1 2 3)) ;; => 3
(.last '(1)) ;; => 1
(.last '() 666) ;; => 666
```

#### .tail `(list &optional (default nil))`

Extract the elements after the head of a list. If list is empty, returns default value.

```common-lisp
(.tail '(1 2 3)) ;; => '(2 3)
(.tail '(1)) ;; => '()
(.tail '() 666) ;; => 666
```

#### .init `(list &optional (default nil))`

Return all the elements of a list except the last one. If list is empty, returns default value.

```common-lisp
(.init '(1 2 3)) ;; => '(1 2)
(.init '(1)) ;; => '()
(.init '() 666) ;; => 666
```

#### .uncons `(list &optional (default nil))`

Decompose a list into its head and tail. If the list is empty, returns default. If the list is non-empty, returns '(x, xs), where x is the head of the list and xs its tail.

```common-lisp
(.uncons '(1 2 3)) ;; => '(1 (2 3))
(.uncons '(1)) ;; => '(1 ())
(.uncons '() 666) ;; => 666
```

### List transformations

#### .map `(fn list)`

Result - is the list obtained by applying `FN` to each element of `LIST`.

```common-lisp
(.map #'1+ '(1 2 3)) ;; => '(2 3 4)
(.map #'1+ '()) ;; => nil
```

#### .intersperse `(sep lisp)`

The `.intersperse` function takes an element and a list and 'intersperses' that element between the elements of the list.

```common-lisp
(.intersperse 0 '(1 2 3)) ;; => '(1 0 2 0 3)
(.intersperse "," '("1" "2" "3")) ;; => '("1" "," "2" "," "3")
(.intersperse 0 '()) ;; => nil
```

#### .intercalate `(sep list)`

`(.intercalate sep list)` is equivalent to `(.concat (.intersperse sep list))`. It inserts the list `SEP` in between the lists in `LIST` and concatenates the result.

```common-lisp
(.intercalate '(0) '((1) (2) (3))) ;; => '(1 0 2 0 3)
(.intercalate '(0) '((1) (2 3) (4 5 6))) ;; => '(1 0 2 3 0 4 5 6)
(.intercalate '(0) '()') ;; => nil
```

### Reducing lists (folds)

#### .foldl `(fn init-val list)`

Left-associative fold of a list.

```common-lisp
(.foldl (lambda (acc x) (append acc (list (1+ x))) '(1 2) '(2 3 4))) ;; => '(1 2 3 4 5)
(.foldl (lambda (acc x) (cons x acc)) '() '(1 2 3)) ;; => '(3 2 1)
(.foldl (lambda (acc x) (cons x acc)) '() '()) ;; => '()
```

#### .foldl1 `(fn list)`

A variant of .foldl that has no base case, and thus may only be applied to non-empty lists.

```common-lisp
(.foldl1 #'+ '(1 2 3)) ;; => 6
(.foldl1 #'list '(1 2 3 4)) ;; => '(((1 2) 3) 4)
```

#### .foldr `(fn init-val list)`

Right-associative fold of a list.

```common-lisp
(.foldr (lambda (x acc) (append acc (list (1+ x))) '(1 2) '(4 3 2))) ;; => '(1 2 3 4 5)
(.foldr (lambda (x acc) (cons x acc)) '() '(1 2 3)) ;; => '(1 2 3)
(.foldr (lambda (x acc) (cons x acc)) '() '()) ;; => '()
```

#### .foldr1 `(fn list)`

A variant of .foldr that has no base case, and thus may only be applied to non-empty lists.

```common-lisp
(.foldr1 #'+ '(1 2 3)) ;; => 6
(.foldr1 #'list '(1 2 3 4)) ;; => '(1 (2 (3 4)))
```

### Special folds

#### .concat `(list)`

The concatenation of all the elements of a container of lists.

```common-lisp
(.concat '((1) (2 3) (4 5 6))) ;; => '(1 2 3 4 5 6)
(.concat '((1)) ;; => '(1)
(.concat '(() ()) ;; => '()
```

#### .concat-map `(fn list)`

Map a function over all the elements of a container and concatenate the resulting lists.

```common-lisp
(.concat-map #'list '(1 2 3)) ;; => '(1 2 3)
(.concat-map (lambda (x) 
              (list x x)) 
             '(1 2 3)) ;; => '(1 1 2 2 3 3)
(.concat-map #'null '()) ;; => NIL
```

#### .and `(list)`

.and returns the conjunction of values in list.


```common-lisp
(.and '(t t)) ;; => t
(.and '(t nil)) ;; => nil
(.and '()) ;; => t
```

#### .or `(list)`

.or returns the disjunction of values in list.

```common-lisp
(.or '(t nil t)) ;; => t
(.or '(nil nil nil)) ;; => nil
(.or '()) ;; => nil
```

#### .any `(fn list)`

Determines whether any element of the list satisfies the predicate.

```common-lisp
(.any #'numberp '("1" "2" 3)) ;; => t
(.any #'numberp '()) ;; => nil
```

#### .all `(fn list)`

Determines whether all elements of the list satisfy the predicate.

```common-lisp
(.all #'numberp '(1 2 3)) ;; => t
(.all #'numberp '(1 "2" 3)) ;; => nil
(.all #'numberp '()) ;; => t
```

#### .sum `(list)`

The .sum function computes the sum of the numbers of a list.

```common-lisp
(.sum '(1 2 3)) ;; => 6
(.sum '()) ;; => 0
```

#### .product `(list)`

The .product function computes the product of the numbers of a list.

```common-lisp
(.product '(1 2 3)) ;; => 6
(.product '()) ;; => 1
```

#### .maximum `(list)`

The largest element of a non-empty list.

```common-lisp
(.maximum '(1 2 3 4 5)) ;; => 5
(.maximum '(1 2 3.0)) ;; => 3.0
```

#### .minimum `(list)`

The least element of a non-empty list.

```common-lisp
(.minimum '(1 2 3 4 5)) ;; => 1
(.minimum '(1.0 2 3)) ;; => 1.0
```

### Building lists

### Scans

#### .scanl `(fn init-value list)`

.scanl is similar to foldl, but returns a list of successive reduced values from the left:

`(.scanl fn init '(x1 x2 ...)) == (list init (fn init x1) (fn (fn init x1) x2) ...)`

```common-lisp
(.scanl #'+ 1 '(2 3 4)) ;; => '(1 3 6 10)
(.scanl #'+ 1 '()) ;; => '(1)
```
#### .scanl1 `(fn list)`

.scanl1 is a variant of .scanl that has no starting value argument.

```common-lisp
(.scanl1 #'+ '(1 2 3 4)) ;; => '(1 3 6 10)
(.scanl1 #'+ '()) ;; => nil
```

#### .scanr `(fn init-value list)`

.scanr is the right-to-left dual of .scanl. 

```common-lisp
(.scanr #'+ 1 '(2 3 4)) ;; => '(10 8 5 1)
(.scanr #'+ 1 '()) ;; => '(1)
```

#### .scanr1 `(fn list)`

.scanr1 is a variant of .scanr that has no starting value argument.

```common-lisp
(.scanr1 #'+ '(2 3 4 1)) ;; => '(10 8 5 1)
(.scanr1 #'+ '(5 4 3 2 1)) ;; => '(15 10 6 3 1)
(.scanr1 #'+ '()) ;; => nil
```

### Accumulating maps

#### .map-accum-l `(fn init-val list)`

.map-accum-l applies a function to each element of a list, passing an accumulating parameter from left to right, and returning a final value of this accumulator together with the new structure.

```common-lisp
(.map-accum-l (lambda (acc x) (list acc (+ x acc))) 1 '(1 2 3)) ;; => '(1 (2 3 4))
(.map-accum-l (lambda (acc x) (list (1+ acc) (+ x acc))) 1 '(1 2 3)) ;; => '(4 (2 4 6))
```

#### .map-accum-r `(fn init-val list)`

.map-accum-r applies a function to each element of a list, passing an accumulating parameter from right to left, and returning a final value of this accumulator together with the new structure.

```common-lisp
(.map-accum-r (lambda (acc x) (list acc (+ x acc))) 1 '(1 2 3)) ;; => '(1 (2 3 4))
(.map-accum-r (lambda (acc x) (list (1+ acc) (+ x acc))) 1 '(1 2 3)) ;; => '(4 (4 4 4))
```

### Infinite lists

#### .iterate `(fn init-val size)`

`(.iterate fn val n)` returns an list of repeated applications of fn to val:

`(.iterate f x size) ==  (list x (f x) (f (f x)) ...)`

Returned list has a size equal to parameter SIZE.

```common-lisp
(.iterate #'1+ 0 4) ;; => '(0 1 2 3)'
(.iterate #'1+ 0 0) ;; => nil
```

#### .repeat `(init-val size)`

`(.repeat x n)` is an list, with x the value of every element.

Returned list has a size equal to parameter SIZE.

```common-lisp
(.repeat 1 4) ;; => '(1 1 1 1)
(.repeat :foo 2) ;; => '(:foo :foo)
(.repeat :foo 0) ;; => nil
```

#### .replicate `(size init-val)`

`(.replicate n x)` is a list of length n with x the value of every element.

```common-lisp
(.replicate 4 1) ;; => '(1 1 1 1)
(.replicate 2 :foo) ;; => '(:foo :foo)
(.replicate 0 :foo) ;; => nil
```

#### .cycle `(list size)`

.cycle ties a finite list into a circular one, or equivalently.

Returned list has a size equal to parameter SIZE.

```common-lisp
(.cycle '(1 2 3) 5) ;; => '(1 2 3 1 2)
(.cycle '(1 2 3) 0) ;; => nil
```

### Unfolding

#### .unfoldr `(fn init-val)`

The .unfoldr function is a dual to .foldr: while .foldr reduces a list to a summary value, .unfoldr builds a list from a seed value. The function takes the element and returns NIL if it is done producing the list or returns '(a b), in which case, a is a prepended to the list and b is used as the next element in a recursive call.

```common-lisp
(.unfoldr (lambda (x) (if (= x 0) nil (list x (1- x)))) 10) ;; => '(10 9 8 7 6 5 4 3 2 1)
(.unfoldr (lambda (x) (if (= x 6) nil (list (expt 2 x) (1+ x)))) 1) ;; => '(2 4 8 16 32)
```

### Sublists


### Extracting sublists

#### .take `(count list)`

`(.take n xs)` returns the prefix of xs of length n, or xs itself if n > length xs.

```common-lisp
(.take 3 '(1 2 3 4 5)) ;; => '(1 2 3)
(.take 3 '(1 2)) ;; => '(1 2)
(.take 3 '()) ;; => nil
(.take -1 '(1 2)) ;; => nil
(.take 0 '(1 2)) ;; => nil
```

#### .drop `(count list)`

`(.drop n xs)` returns the suffix of xs after the first n elements, or NIL if n > length xs.

```common-lisp
(.drop 3 '(1 2 3 4 5)) ;; => '(4 5)
(.drop 3 '(1 2)) ;; => nil
(.drop 3 '()) ;; => nil
(.drop -1 '(1 2)) ;; => '(1 2)
(.drop 0 '(1 2)) ;; => '(1 2)
```

#### .split-at `(count list)`

`splitAt n xs` returns a list where first element is xs prefix of length n and second element is the remainder of the list.

```common-lisp
(.split-at 3 '(1 2 3 4 5)) ;; =>  '((1 2 3) (4 5))
(.split-at 1 '(1 2 3) ;; => '((1) (2 3))
(.split-at 3 '(1 2 3) ;; => '((1 2 3) nil)
(.split-at 4 '(1 2 3) ;; => '((1 2 3) ())
(.split-at 0 '(1 2 3) ;; => '(nil (1 2 3))
(.split-at -1 '(1 2 3) ;; => '(nil (1 2 3))
```

#### .take-while `(pred list)`

.take-while, applied to a predicate PRED and a LIST, returns the longest prefix (possibly empty) of LIST of elements that satisfy PRED.

```common-lisp
(.take-while #'evenp '(1 2 3 4)) ;; => '()
(.take-while #'evenp '(2 4 5 6)) ;; => '(2 4)
```

#### .drop-while `(pred list)`

`.drop-while p xs` returns the suffix remaining after `.take-while p xs`.

```common-lisp
(.drop-while #'numberp '(1 2 3 nil nil 1 2 3)) ;; => '(nil nil 1 2 3)
(.drop-while #'numberp '(1 2 3)) ;; => '()
(.drop-while #'stringp '(1 2 3)) ;; => '(1 2 3)
```

#### .drop-while-end `(pred list)`

The .drop-while-end function drops the largest suffix of a list in which the given predicate holds for all elements.

```common-lisp
(.drop-while-end #'numberp '("foo" "bar" 1 2 3)) ;; => '("foo" "bar")
(.drop-while-end #'numberp '("foo" 1 2 3 "bar")) ;; => '("foo" 1 2 3 "bar")
(.drop-while-end #'numberp '(1 2 3)) ;; => '()
```

#### .span `(pred list)`

.span, applied to a predicate PRED and a LIST, returns a list where first element is longest prefix (possibly empty) of LIST of elements that satisfy PRED and second element is the remainder of the list.

```common-lisp
(.span (lambda (x) (< x 3)) '(1 2 3 4 1 2 3 4)) ;; => '((1 2) (3 4 1 2 3 4))
(.span (lambda (x) (< x 9)) '(1 2 3)) ;; => '((1 2 3) ())
(.span (lambda (x) (< x 0)) '(1 2 3)) ;; => '(() (1 2 3))
```

#### .break `(pred list)`

.break, applied to a predicate PRED and a LIST, returns a list where first element is longest prefix (possibly empty) of LIST of elements that do not satisfy PRED and second element is the remainder of the list

```common-lisp
(.break (lambda (x) (> x 3)) '(1 2 3 4 1 2 3 4)) ;; => '((1 2 3) (4 1 2 3 4))
(.break (lambda (x) (< x 9)) '(1 2 3)) ;; => '(() (1 2 3))
(.break (lambda (x) (> x 9)) '(1 2 3)) ;; => '((1 2 3) ())
```

#### .strip-prefix `(prefix list &optional (default nil))`

The .strip-prefix function drops the given prefix from a list. It returns DEFAULT value if the list did not start with the prefix given, or the list after the prefix, if it does.

```common-lisp
(.strip-prefix '(1 2) '(1 2 3 4)) ;; => '(3 4)
(.strip-prefix '(1 2) '(1 2)) ;; => '()
(.strip-prefix '(1 2) '(3 4 1 2)) ;; => NIL
(.strip-prefix '(1 2) '(3 4 1 2 5 6)) ;; => NIL
```

#### .inits `(list)`

The .inits function returns all initial segments of the argument, shortest first.

```common-lisp
(.inits '(1 2 3)) ;; => '(nil (1) (1 2) (1 2 3))
(.inits '()) ;; => '(nil)
```

#### .tails `(list)`

The .tails function returns all final segments of the argument, longest first.

```common-lisp
(.tails '(1 2 3)) ;; => '((1 2 3) (2 3) (3) nil)
(.tails '()) ;; => '(nil)
```

### Predicates

#### .is-prefix-of `(prefix list)`

The .is-prefix-of function takes two lists and returns `T` if the first list is a prefix of the second.

```common-lisp
(.is-prefix-of '(1 2) '(1 2 3 4)) ;; => T
(.is-prefix-of '(1 2) '(4 3 2 1)) ;; => nil
(.is-prefix-of '() '(1 2 3)) ;; => T
```

#### .is-suffix-of `(suffix list)`

The .is-suffix-of function takes two lists and returns `T` if the first list is a suffix of the second. 

```common-lisp
(.is-suffix-of '(2 1) '(4 3 2 1)) ;; => T
(.is-suffix-of '(1 2) '(4 3 2 1)) ;; => nil
(.is-suffix-of '() '(1 2 3)) ;; => T
```

#### .is-infix-of `(infix list)`

The .is-infix-of function takes two lists and returns `T` if the first list is contained, wholly and intact, anywhere within the second.

```common-lisp
(.is-infix-of '(1 2) '(3 3 1 2 3 3)) ;; => T
(.is-infix-of '(1 2 3) '(4 1 2 4 3)) ;; => nil
(.is-infix-of '() '(1 2 3)) ;; => T
```

#### .is-subsequence-of `(subseq list)`

The .is-subsequence-of function takes two lists and returns `T` if all the elements of the first list occur, in order, in the second. The elements do not have to occur consecutively.


```common-lisp
(.is-subsequence-of '(1 2 3) '(1 0 2 0 3 0)) ;; => T
(.is-subsequence-of '(1 2 3) '(1 0 2 0 4 0)) ;; => nil
(.is-subsequence-of '() '(1 2 3)) ;; => T
```


### Searching lists


### Searching by equality

#### .elem `(element list &key (test 'equalp))`

Does the element occur in the structure?

```common-lisp
(.elem 1 '(1 2 3)) ;; => T
(.elem "one" '(1 "one" 3) :test 'eql) ;; => nil
```

#### .not-elem `(element list &key (test 'equalp))`

.not-elem is the negation of .elem.

```common-lisp
(.not-elem 7 '(1 2 3)) ;; => T
(.not-elem "one" '(1 "one" 3) :test 'eql) ;; => T
```

### Searching with a predicate

#### .find `(fn list &optional (default nil))`

The .find function takes a predicate and a list and returns the leftmost element of the list matching the predicate, or `default` argument if there is no such element.

```common-lisp
(.find #'numberp '(:foo :bar 1 2 3)) ;; => 1
(.find #'numberp '(:foo :bar)) ;; => nil
(.find #'numberp '(:foo :bar) 666) ;; => 666
```


## Author

* Ito Dimercel (xolcman@gmail.com)

## Copyright

Copyright (c) 2018 Ito Dimercel (xolcman@gmail.com)

## License

Licensed under the LLGPL License.
