# Listopia

 This is port of Haskell package [Data.List](https://hackage.haskell.org/package/base-4.10.1.0/docs/Data-List.html)

## Usage

```common-lisp
(:use :listopia)
```

## Installation


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

#### .foldr `(fn init-val list)`

Right-associative fold of a list.

```common-lisp
(.foldr (lambda (x acc) (append acc (list (1+ x))) '(1 2) '(4 3 2))) ;; => '(1 2 3 4 5)
(.foldr (lambda (x acc) (cons x acc)) '() '(1 2 3)) ;; => '(1 2 3)
(.foldr (lambda (x acc) (cons x acc)) '() '()) ;; => '()
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

### Special folds

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

## Author

* Ito Dimercel (xolcman@gmail.com)

## Copyright

Copyright (c) 2018 Ito Dimercel (xolcman@gmail.com)

## License

Licensed under the LLGPL License.
