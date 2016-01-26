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

    (defmacro define-matrix-type (numeric-type))

For example, to create functions for `SINGLE-FLOAT` matrices optimized
for speed, one might:

    (locally
        (declare (optimize (speed 3) (safety 1)))
      (in-package :gauss)
      (define-matrix-type single-float))

ISSUE: There is currently a limitation which requires one to use this
macro in the `GAUSS` package.

<a name="create">Creating matrices and vectors</a>
--------------------------------------------------

One can create a matrix from a list of values or from explicit values:

     (defun make-matrix (type-list rows cols list-of-values) ...)
     (defun make-matrix* (type-list rows cols &rest values) ...)

Similarly, one can create a vector from a list of values or from
explicit values:

     (defun make-vector (type-list list-of-values) ...)
     (defun make-vector* (type-list &rest values) ...)

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

One can add matrices with `M+` and multiply them with `M*`.  The list
of types for these functions contains two types, one for the first
matrix and one for the second matrix.

    (defun m+ (types-list matrix-a matrix-b) ...)
    (defun m* (types-list matrix-a matrix-b) ...)

For example:

    (let ((m (gauss:make-matrix* '(single-float)
                                 2 2
                                 1.0 2.0
                                 3.0 4.0))
          (v (gauss:make-vector* '(single-float) 0.5 0.5)))
      (gauss:m* '(single-float single-float)
                m
                (gauss:m+ '(single-float single-float) v v)))

Which yields:

    #<MATRIX
      3.0
      7.0>

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
