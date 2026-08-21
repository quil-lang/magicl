(in-package #:magicl.foreign-libraries)

(cffi:define-foreign-library liblapack
  #+:magicl.use-accelerate
  (:darwin "libLAPACK.dylib" :search-path #P"/System/Library/Frameworks/Accelerate.framework/Frameworks/vecLib.framework/Versions/A/")
  ;; OpenBLAS first: it supplies a complete LAPACK, and on arm64 macOS it is the
  ;; only one of these that is currently both complete and numerically correct.
  ;; Homebrew's reference LAPACK next, since it is more complete than the system
  ;; LAPACK, which lacks many of the more obscure subroutines. A bare
  ;; liblapack.dylib last -- on macOS that resolves to Accelerate.
  #-:magicl.use-accelerate
  (:darwin (:or
            "/opt/homebrew/opt/openblas/lib/libopenblas.dylib"
            "/usr/local/opt/openblas/lib/libopenblas.dylib"
            "/opt/homebrew/opt/lapack/lib/liblapack.dylib"
            "/usr/local/opt/lapack/lib/liblapack.dylib"
            "liblapack.dylib"))
  #+:magicl.use-mkl
  (:unix  "libmkl_rt.so")
  #-:magicl.use-mkl
  (:unix  (:or "libopenblas.so"
               "libopenblas.so.0"
               "liblapack.so"
               "liblapack.so.3"))
  (t (:default "liblapack")))

(pushnew 'liblapack *foreign-libraries*)
(export 'liblapack)

(defvar *blapack-libs-loaded* nil)

(unless *blapack-libs-loaded*
  (cffi:load-foreign-library 'liblapack)
  (setf *blapack-libs-loaded* t))

(magicl:define-backend :lapack
  :documentation "Backend for LAPACK functionality written in Fortran."
  :default t)
