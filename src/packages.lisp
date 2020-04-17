(defpackage #:magicl.foreign-libraries
  (:use #:common-lisp)
  (:export #:libgfortran
           #:libblas
           #:liblapack
           #:libexpokit
           #:foreign-symbol-available-p
           #:print-availability-report))

(defpackage #:magicl.cffi-types
  (:use #:common-lisp
        #:cffi)
  (:export #+sbcl #:array-pointer       ; FUNCTION
           #:with-array-pointers        ; MACRO
           #:complex-single-float
           #:complex-double-float
           #:fortran-int
           #:fortran-float
           #:fortran-double
           #:fortran-complex-float
           #:fortran-complex-double
           #:fortran-logical))

(defpackage #:magicl.blas-cffi
  (:use)
  #-package-local-nicknames
  (:nicknames #:blas))

(defpackage #:magicl.lapack-cffi
  (:use)
  #-package-local-nicknames
  (:nicknames #:lapack))

(defpackage #:magicl.expokit-cffi
  (:use)
  #-package-local-nicknames
  (:nicknames #:expokit))

(defpackage #:magicl
  (:use #:common-lisp
        #:cffi
        #:abstract-classes)
  #+package-local-nicknames
  (:local-nicknames (#:blas #:magicl.blas-cffi)
                    (#:lapack #:magicl.lapack-cffi)
                    (#:expokit #:magicl.expokit-cffi))
  (:import-from #:magicl.foreign-libraries
                #:print-availability-report)
  (:shadow #:vector
           #:=
           #:map
           #:trace
           #:every
           #:some
           #:notevery
           #:notany)
  (:export #:with-blapack
           
           ;; abstract-tensor protocol
           #:specialize-tensor
           #:generalize-tensor
           #:shape
           #:tref
           #:order
           #:size
           #:element-type
           #:lisp-array

           #:every
           #:some
           #:notevery
           #:notany

           #:map
           #:map!

           #:reshape
           #:slice
           
           ;; Classes
           #:tensor
           #:matrix

           ;; Accessors
           #:nrows
           #:ncols

           ;; Subtypes
           #:tensor/single-float
           #:tensor/double-float
           #:tensor/complex-single-float
           #:tensor/complex-double-float
           #:matrix/single-float
           #:matrix/double-float
           #:matrix/complex-single-float
           #:matrix/complex-double-float
           #:column-vector/single-float
           #:column-vector/double-float
           #:column-vector/complex-single-float
           #:column-vector/complex-double-float
           #:row-vector/single-float
           #:row-vector/double-float
           #:row-vector/complex-single-float
           #:row-vector/complex-double-float
           #:vector/single-float
           #:vector/double-float
           #:vector/complex-single-float
           #:vector/complex-double-float

           ;; Constructors
           #:make-tensor
           #:empty
           #:const
           #:rand
           #:eye
           #:arange
           #:from-array
           #:from-list
           #:from-diag
           #:zeros
           #:ones

           #:random-unitary

           ;; Operators
           #:binary-operator
           #:.+
           #:.-
           #:.*
           #:./
           #:.^
           #:=
           #:map
           
           ;; Matrix operators
           #:square-matrix-p
           #:identity-matrix-p
           #:unitary-matrix-p
           #:row
           #:column
           #:@
           #:mult
           #:kron
           #:scale
           #:scale!
           #:diag
           #:det
           #:upper-triangular
           #:lower-triangular
           #:triu
           #:tril
           #:transpose
           #:transpose!
           #:orthonormalize
           #:orthonormalize!
           #:trace
           #:direct-sum
           #:conjugate-transpose
           #:conjugate-transpose!
           #:dagger
           #:dagger!
           #:eig
           #:hermitian-eig
           #:inv
           #:lu
           #:csd
           #:svd
           #:ql
           #:qr
           #:rq
           #:lq
           
           #:polynomial
           #:make-polynomial
           #:polynomial-coefficients
           #:polynomial-solve
           #:polynomial-eval
           #:polynomial-diff
           #:polynomial-newton-iteration

           ;; Vector operators
           #:dot
           #:norm

           ;; LAPACK stuff
           #:lapack-eig
           #:lapack-lu
           #:lapack-csd
           #:lapack-svd
           #:lapack-ql
           #:lapack-qr
           #:lapack-rq
           #:lapack-lq
           #:lapack-ql-q
           #:lapack-qr-q
           #:lapack-rq-q
           #:lapack-lq-q
           ))
