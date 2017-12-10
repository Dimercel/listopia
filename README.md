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

## Author

* Ito Dimercel (xolcman@gmail.com)

## Copyright

Copyright (c) 2017 Ito Dimercel (xolcman@gmail.com)

## License

Licensed under the LLGPL License.
