(in-package #:magicl.foreign-libraries)

(cffi:define-foreign-library libblas
  #+:magicl.use-accelerate
  (:darwin "libBLAS.dylib" :search-path #P"/System/Library/Frameworks/Accelerate.framework/Frameworks/vecLib.framework/Versions/A/")
  ;; Kept in step with liblapack's search order: pairing BLAS and LAPACK from
  ;; different implementations is a common source of subtle wrong answers.
  #-:magicl.use-accelerate
  (:darwin (:or
            "/opt/homebrew/opt/openblas/lib/libopenblas.dylib"
            "/usr/local/opt/openblas/lib/libopenblas.dylib"
            "/opt/homebrew/opt/lapack/lib/libblas.dylib"
            "/usr/local/opt/lapack/lib/libblas.dylib"
            "libblas.dylib" ))
  #+:magicl.use-mkl
  (:unix  "libmkl_rt.so")
  #-:magicl.use-mkl
  (:unix  (:or "libopenblas.so"
               "libopenblas.so.0"
               "libblas.so"
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
  :default t)
