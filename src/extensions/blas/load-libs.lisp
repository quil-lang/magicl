(in-package #:magicl.foreign-libraries)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (cffi:define-foreign-library libblas
    #+:magicl.use-accelerate
    (:darwin "libBLAS.dylib" :search-path #P"/System/Library/Frameworks/Accelerate.framework/Frameworks/vecLib.framework/Versions/A/")
    #-:magicl.use-accelerate
    (:darwin (:or "/usr/local/opt/lapack/lib/libblas.dylib" "libblas.dylib" ))
    #+:magicl.use-mkl
    (:unix  "libmkl_rt.so")
    #-:magicl.use-mkl
    (:unix  (:or "libblas.so"
                 "libblas.so.3"))
    (t (:default "libblas")))

  (pushnew 'libblas *foreign-libraries*)
  (export 'libblas)

  (defvar *blas-loaded* nil)

  (unless *blas-loaded*
    (cffi:load-foreign-library 'libblas)
    (setf *blas-loaded* t))

  (magicl:define-backend :blas
    :documentation "Backend for BLAS functionality written in Fortran."
    :default t))
