;;;; lapack-bindings.lisp
;;;;
;;;; Author: Cole Scott

(in-package #:magicl)

(macrolet ((def-all-lapack ()
             "Define the lapack bindings for all lapack types"
             `(progn
                ;; Single-float
                ,(def-lapack-mult-for-type 'matrix/single-float 'single-float 'magicl.blas-cffi:%sgemm)
                ,(def-lapack-lu-for-type 'matrix/single-float 'single-float 'magicl.lapack-cffi:%sgetrf)
                ,(def-lapack-inv-for-type 'matrix/single-float 'single-float 'magicl.lapack-cffi:%sgetrf 'magicl.lapack-cffi:%sgetri)
                ,(def-lapack-svd-for-type 'matrix/single-float 'single-float 'magicl.lapack-cffi:%sgesvd)
                ,(def-lapack-eig-for-type 'matrix/single-float 'single-float 'magicl.lapack-cffi:%sgeev)
                ,(def-lapack-ql-qr-rq-lq-for-type 'matrix/single-float 'single-float
                   'magicl.lapack-cffi:%sgeqlf 'magicl.lapack-cffi:%sgeqrf 'magicl.lapack-cffi:%sgerqf 'magicl.lapack-cffi:%sgelqf
                   'magicl.lapack-cffi:%sorgql 'magicl.lapack-cffi:%sorgqr 'magicl.lapack-cffi:%sorgrq 'magicl.lapack-cffi:%sorglq)
                ;; Double-float
                ,(def-lapack-mult-for-type 'matrix/double-float 'double-float 'magicl.blas-cffi:%dgemm)
                ,(def-lapack-lu-for-type 'matrix/double-float 'double-float 'magicl.lapack-cffi:%dgetrf)
                ,(def-lapack-inv-for-type 'matrix/double-float 'double-float 'magicl.lapack-cffi:%dgetrf 'magicl.lapack-cffi:%dgetri)
                ,(def-lapack-svd-for-type 'matrix/double-float 'double-float 'magicl.lapack-cffi:%dgesvd)
                ,(def-lapack-eig-for-type 'matrix/double-float 'double-float 'magicl.lapack-cffi:%dgeev)
                ,(def-lapack-ql-qr-rq-lq-for-type 'matrix/double-float 'double-float
                   'magicl.lapack-cffi:%dgeqlf 'magicl.lapack-cffi:%dgeqrf 'magicl.lapack-cffi:%dgerqf 'magicl.lapack-cffi:%dgelqf
                   'magicl.lapack-cffi:%dorgql 'magicl.lapack-cffi:%dorgqr 'magicl.lapack-cffi:%dorgrq 'magicl.lapack-cffi:%dorglq)
                ;; Complex single-float
                ,(def-lapack-mult-for-type 'matrix/complex-single-float '(complex single-float) 'magicl.blas-cffi:%cgemm)
                ,(def-lapack-lu-for-type 'matrix/complex-single-float '(complex single-float) 'magicl.lapack-cffi:%cgetrf)
                ,(def-lapack-inv-for-type 'matrix/complex-single-float '(complex single-float) 'magicl.lapack-cffi:%cgetrf 'magicl.lapack-cffi:%cgetri)
                ,(def-lapack-svd-for-type 'matrix/complex-single-float '(complex single-float) 'magicl.lapack-cffi:%cgesvd 'single-float)
                ,(def-lapack-eig-for-type 'matrix/complex-single-float '(complex single-float) 'magicl.lapack-cffi:%cgeev 'single-float)
                ,(def-lapack-hermitian-eig-for-type 'matrix/complex-single-float '(complex single-float) 'magicl.lapack-cffi:%cheev 'single-float)
                ,(def-lapack-ql-qr-rq-lq-for-type 'matrix/complex-single-float '(complex single-float)
                   'magicl.lapack-cffi:%cgeqlf 'magicl.lapack-cffi:%cgeqrf 'magicl.lapack-cffi:%cgerqf 'magicl.lapack-cffi:%cgelqf
                   'magicl.lapack-cffi:%cungql 'magicl.lapack-cffi:%cungqr 'magicl.lapack-cffi:%cungrq 'magicl.lapack-cffi:%cunglq)
                ;; Complex double-float
                ,(def-lapack-mult-for-type 'matrix/complex-double-float '(complex double-float) 'magicl.blas-cffi:%zgemm)
                ,(def-lapack-lu-for-type 'matrix/complex-double-float '(complex double-float) 'magicl.lapack-cffi:%zgetrf)
                ,(def-lapack-inv-for-type 'matrix/complex-double-float '(complex double-float) 'magicl.lapack-cffi:%zgetrf 'magicl.lapack-cffi:%zgetri)
                ,(def-lapack-svd-for-type 'matrix/complex-double-float '(complex double-float) 'magicl.lapack-cffi:%zgesvd 'double-float)
                ,(def-lapack-eig-for-type 'matrix/complex-double-float '(complex double-float) 'magicl.lapack-cffi:%zgeev 'double-float)
                ,(def-lapack-hermitian-eig-for-type 'matrix/complex-double-float '(complex double-float) 'magicl.lapack-cffi:%zheev 'double-float)
                ,(def-lapack-ql-qr-rq-lq-for-type 'matrix/complex-double-float '(complex double-float)
                   'magicl.lapack-cffi:%zgeqlf 'magicl.lapack-cffi:%zgeqrf 'magicl.lapack-cffi:%zgerqf 'magicl.lapack-cffi:%zgelqf
                   'magicl.lapack-cffi:%zungql 'magicl.lapack-cffi:%zungqr 'magicl.lapack-cffi:%zungrq 'magicl.lapack-cffi:%zunglq))))
  (def-all-lapack))
