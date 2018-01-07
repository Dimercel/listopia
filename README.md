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

## Author

* Ito Dimercel (xolcman@gmail.com)

## Copyright

Copyright (c) 2017 Ito Dimercel (xolcman@gmail.com)

## License

Licensed under the LLGPL License.
