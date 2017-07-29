(in-package #:magicl)

;; Import/Export BLAS-CFFI symbols
(do-external-symbols (s :blas-cffi)
  (export s))

;; LAPACK-CFFI is different:
;; don't use the package LAPACK-CFFI directly since some functions
;; such as %XERBLA and %LSAME are defined in both LAPACK and BLAS and
;; we want to use the BLAS versions.
(do-external-symbols (s :lapack-cffi)
  (unless (find-symbol (symbol-name s) :magicl)
    (import s)
    (export s)))


;; There is one more export issue -- take a look at some of the issues
;; with the linpack stuff in CLS. 


;; Right now, only works with SBCL/CMUCL.
;; Please add appropriate definitions for alternate implementations.

#+sbcl
(defmacro with-blapack (&body body)
  `(sb-int:with-float-traps-masked (:divide-by-zero)
    ,@body))

#+cmu
(defmacro with-blapack (&body body)
  `(extensions:with-float-traps-masked (:divide-by-zero)
     ,@body))

#-(or sbcl cmu)
(defmacro with-blapack (&body body)
  `(progn
    ,@body))
