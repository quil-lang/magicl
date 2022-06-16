(in-package #:cl)

(defpackage #:magicl.backends
  (:use #:cl)
  (:export
   #:no-applicable-implementation
   #:define-compatible-no-applicable-method-behavior

   #:*backend*
   #:active-backends
   #:define-backend
   #:with-backends

   #:backend-function-p
   #:backend-implementation

   #:define-backend-function
   #:define-backend-implementation))

(defpackage #:magicl
  (:use #:common-lisp
        #:abstract-classes)
  (:shadow #:vector
           #:=
           #:map
           #:trace
           #:every
           #:some
           #:notevery
           #:notany
           #:make-array)

  (:import-from #:magicl.backends
                #:no-applicable-implementation
                #:define-compatible-no-applicable-method-behavior
                #:define-backend
                #:define-backend-function
                #:define-backend-implementation)
  (:export #:no-applicable-implementation
           #:define-compatible-no-applicable-method-behavior
           #:define-backend
           #:define-backend-function
           #:define-backend-implementation)

  (:export #:with-blapack
           #:time-backends
           #:define-extensible-function ; For extensions, not users...

           #:allocate-storage
           #:*default-allocator*

           ;; abstract-tensor protocol
           #:abstract-tensor
           #:specialize-tensor
           #:generalize-tensor
           #:shape
           #:layout
           #:copy-tensor
           #:deep-copy-tensor
           #:cast
           #:tref
           #:order
           #:size
           #:element-type
           #:lisp-array

           #:every
           #:some
           #:notevery
           #:notany
           #:make-array

           #:map
           #:map!

           #:reshape
           #:slice
           #:slice-to

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

           #:random-normal
           #:random-unitary
           #:random-special-unitary
           #:random-hermitian

           ;; Block Matrix Constructors
           #:block-diag
           #:hstack
           #:vstack
           #:block-matrix

           ;; Operators
           #:binary-operator
           #:unary-operator
           #:.+
           #:.-
           #:.*
           #:./
           #:.^
           #:=
           #:map
           #:.exp
           #:.log
           #:.max
           #:.min

           ;; Matrix operators
           #:square-matrix-p
           #:identity-matrix-p
           #:unitary-matrix-p
           #:hermitian-matrix-p
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
           #:lu-solve
           #:csd-blocks
           #:csd
           #:svd
           #:qz
           #:ql
           #:qr
           #:rq
           #:lq
           #:expm
           #:logm
           #:expih
           #:linear-solve

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
           #:normalize
           #:normalize!

           ;; Misc utilities
           #:row-major-index
           #:matrix-row-major-index
           #:column-major-index
           #:matrix-column-major-index
           #:valid-index-p
           #:vector->row-matrix
           #:vector->column-matrix
           #:row-matrix->vector
           #:column-matrix->vector))
