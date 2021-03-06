GAUSS
=====

* [Overview](#overview)
* [Future Directions](#future)
* [Defining matrices for a given numeric type](#define)
* [Creating matrices and vectors](#create)
* [Accessing attributes of matrices and vectors](#attribs)
* [Indexing into matrices and vectors](#indexing)
* [Transposing a matrix](#transpose)
* [Adding and multiplying matrices and vectors](#add-mul)
* [Solving linear equations](#solve)
* [Shortcuts](#shortcuts)

<a name="overview">Overview</a>
-------------------------------

This package provides simple matrix manipulation in Common Lisp.
There are already a number of packages that do basic matrix
manipulation in Common Lisp.  You will prefer this package to the
other such packages if:

* You do not want to rely on FFI to BLAS
* Boxing and unboxing of numeric types is too much overhead
* Generic function dispatch is too much overhead

This package employs Robert Smith's [Template][template] library to
avoid boxing and unboxing as much as possible.  As such, many of the
functions require a specified list of types.  This allows compile-time
dispatch of the appropriately typed function (when the given list of
types is known at compile-time).

  [template]: https://bitbucket.org/tarballs_are_good/template

All of the public functionality of this library treats matrices and
vectors as immutable.  This library does not guarantee correct
behavior if one takes it upon oneself to circumvent that immutability.
In particular, at the moment, if one takes the tranpose of a matrix
and then modifies it, the original matrix will also be modified.

<a name="future">Future Directions</a>
--------------------------------------

This library currently only features the functionality that I
need to implement Kalman filtering.  Features that I expect to want
out of this library in the not-too-distant future (tomorrow A.D.) are:

* Solving multiple linear-equations with the same matrix
* Eigenvectors and eigenvalues
* QR decomposition
* LU decomposition
* Singular-value decomposition

<a name="define">Defining matrices for a given numeric type</a>
---------------------------------------------------------------

To create the functions needed matrices of a given numeric type, one
uses the `DEFINE-MATRIX-TYPE` macro:

    (defmacro define-matrix-type (numeric-type) ...)

For example, to create functions for `SINGLE-FLOAT` matrices optimized
for speed, one might:

    (in-package :gauss)
    (locally
        (declare (optimize (speed 3) (safety 1)))
      (define-matrix-type single-float))

ISSUE: There is currently a limitation which requires one to use this
macro in the `GAUSS` package.

The `DEFINE-MATRIX-TYPE` creates all of the functions needed for
operating with matrices of the given type.  If one wishes to mix
matrix types (e.g. multiplying a single-float matrix by a double-float
vector), one needs to define the mixed-type operations with:

     (defmacro define-mixed-type-matrix-operations (type-a type-b) ...)

For example:

    (in-package :gauss)
    (locally
        (declare (optimize (speed 3) (safety 1)))
      (define-mixed-type-matrix-operations single-float double-float))

ISSUE: There is currently a limitation which requires one to use this
macro in the `GAUSS` package.

The `GAUSS` package itself declares everything needed to use
`rational`, `single-float`, or `double-float` matrices, but it does
not define any of the operations for mixing and matching between those
types.

    (define-matrix-type rational)
    (define-matrix-type single-float)
    (define-matrix-type double-float)

Those operations in the `GAUSS` package are compiled with `(speed 2)`
and `(safety 3)`.  Most of the operations in the `GAUSS` package are
such that run-time validation of parameters is omitted if `speed` is
greater than `safety`.  I recommend that you use the pre-compiled
settings while coding/debugging and use a `LOCALLY` block as above
to recompile them with `speed` greater than `safety` once you have the
kinks worked out.

As an example, the following form will cause an assertion if `speed`
is less than or equal to `safety` at the time the matrix operations
are compiled, but will slide through otherwise:

    (gauss:m+ '(single-float single-float)
              (gauss:make-vector* '(single-float) 1.0)
              (gauss:make-vector* '(single-float) 2.0 5.0))

When it slides through, it currently creates a 1x1 matrix:

    #<MATRIX
      3.0>

However, I do not guarantee that any behavior which would `ASSERT`
when compiled with `speed` less than or equal to `safety` will behave
consistently across versions or compilers when `speed` is greater than
`safety`.  In other words, if your code does not work when compiled
with `speed` less than or equal to `safety`, then you should not be
relying on the results you obtain when `speed` is greater than
`safety`.

<a name="create">Creating matrices and vectors</a>
--------------------------------------------------

One can create a matrix from a list of values or from explicit values:

     (defun make-matrix (type-list rows cols list-of-values) ...)
     (defun make-matrix* (type-list rows cols &rest values) ...)

Similarly, one can create a vector from a list of values or from
explicit values:

     (defun make-vector (type-list list-of-values) ...)
     (defun make-vector* (type-list &rest values) ...)

All of the above return a structure of type `MATRIX`.

For example, to create a `SINGLE-FLOAT` matrix and a commensurate
vector, one might:

    (let ((matrix (gauss:make-matrix '(single-float)
                                     2 3
                                     '(1.0 2.0 3.0
                                       2.0 3.0 4.0)))
          (vector (gauss:make-vector* '(single-float) 1.5 2.5)))
      (values matrix vector))

Which would yield the values:

    #<MATRIX
      1.0  2.0  3.0
      2.0  3.0  4.0>
    #<MATRIX
      1.5
      2.5>

<a name="attribs">Accessing attributes of matrices and vectors</a>
------------------------------------------------------------------

One can query the number of rows and columns in a matrix:

    (defun mrows (matrix) ...)
    (defun mcols (matrix) ...)

One can query the numeric type of a matrix:

    (defun mtype (matrix) ...)

The library also defines several predicates which can be useful when
asserting pre-conditions:

    (defun square-matrix-p (matrix) ...)
    (defun commensuratep (matrix-a matrix-b) ...)
    (defun column-vector-p (matrix) ...)

The function `COMMENSURATEP` returns true if one could multiply
`MATRIX-A` by `MATRIX-B`, in that order.  In other words, the number
of columns in `MATRIX-A` must equal the number of rows in `MATRIX-B`.

<a name="indexing">Indexing into matrices and vectors</a>
---------------------------------------------------------

To retrieve elements from a matrix, one can use `MREF`.  To retrieve
elements from a vector, one can use `VREF`.  To retrieve elements from
a transposed vector, one can use `VTREF`.

    (defun mref (types-list matrix row col) ...)
    (defun vref (types-list column-vector row) ...)
    (defun vtref (types-list row-vector col) ...)

For example:

    (let ((vector (gauss:make-vector* '(single-float) 1.5 2.5)))
      (+ (gauss:vref '(single-float) vector 0)
         (gauss:mref '(single-float) vector 1 0)))

<a name="transpose">Transposing a matrix</a>
--------------------------------------------

The `TRANSPOSE` function can be used to calculate the transpose of a
matrix.

    (defun transpose (type-list matrix) ...)

For example:

    (gauss:transpose '(single-float)
                     (gauss:make-vector* '(single-float) 1.0 2.0 3.0 4.0))

Which yields:

    #<MATRIX
      1.0  2.0  3.0  4.0>

<a name="add-mul">Adding and multiplying matrices and vectors</a>
---------------------------------------------------------------

One can add matrices with `M+` (or `V+`), subtract matrices with `M-`
(or `V-`), and multiply them with `M*`.  The list of types for these
functions contains two types, one for the first matrix and one for the
second matrix.

    (defun m+ (types-list matrix-a matrix-b) ...)
    (defun m- (types-list matrix-a matrix-b) ...)
    (defun m* (types-list matrix-a matrix-b) ...)

    (defun v+ (types-list vector-a vector-b) ...)
    (defun v- (types-list vector-a vector-b) ...)

For example:

    (let ((m (gauss:make-matrix* '(single-float)
                                 2 2
                                 1.0 2.0
                                 3.0 4.0))
          (v (gauss:make-vector* '(single-float) 0.5 0.5)))
      (gauss:m* '(single-float single-float)
                m
                (gauss:v+ '(single-float single-float) v v)))

Which yields:

    #<MATRIX
      3.0
      7.0>

One can also scale a matrix by a scalar factor:

    (defun scale (types-list matrix scalar) ...)

For example:

    (let ((v (gauss:make-vector* '(single-float) 1.0 2.0)))
      (gauss:scale '(single-float single-float) v 3.0))

Which yields:

    #<MATRIX
      3.0
      6.0>


<a name="solve">Solving linear equations</a>
--------------------------------------------

One can use the `SOLVE` function to solve a system of linear
equations.  Given a matrix `M` and a commensurate vector `V`, the
`SOLVE` function returns a vector `X` such that `M * X = V`.

     (defun solve (types-list matrix vector) ...)

The `TYPES-LIST` is a two-element list specifying (first) the numeric
type of the matrix and (second) the numeric type of the vector.

For example:

    (let ((m (gauss:make-matrix* '(single-float)
                                 2 2
                                 1.0 2.0
                                 3.0 4.0))
          (v (gauss:make-vector* '(single-float) 3.0 5.0)))
      (gauss:solve '(single-float single-float) m v))

Which yields:

    #<MATRIX
      -1.0
      2.0>

<a name="shortcuts">Shortcuts</a>
---------------------------------

One can define shortcuts so that one need not include such verbose
type information.  One creates shortcuts using the
`DEFINE-MATRIX-OPERATION-SHORTCUTS` macro:

    (defmacro define-matrix-operation-shortcuts (ext-a type-a ext-b type-b)
       ...)

This will create functions: `MAKE-MATRIX/A`, `MREF/A`, `M+/AB`,
`M+/BA`, etc. where `A` and `B` are the given extensions.  All
shortcuts are created in the same package as `EXT-A`.

Note: no shortcuts are made for the functions which do not require a
list of types.  There is no `MROWS/A` defined, for example.

The `GAUSS` package exports shortcuts for `RATIONAL`, `SINGLE-FLOAT`,
and `DOUBLE-FLOAT`.

    (define-matrix-operation-shortcuts q rational q rational)
    (define-matrix-operation-shortcuts s single-float s single-float)
    (define-matrix-operation-shortcuts d double-float d double-float)

For example, one could abbreviate the code given in the
[previous section](#solve) as:

    (let ((m (gauss:make-matrix*/s 2 2
                                   1.0 2.0
                                   3.0 4.0))
          (v (gauss:make-vector*/s 3.0 5.0)))
      (gauss:solve/ss m v))

Which yields:

    #<MATRIX
      -1.0
      2.0>
