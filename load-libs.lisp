(in-package #:magicl.foreign-libraries)

;; useful command: nm -jgU

#+(or (not :darwin) (not :magicl.use-accelerate))
(cffi:define-foreign-library libgfortran
  (:darwin "libgfortran.dylib" :search-path #P"/usr/local/opt/gcc/lib/gcc/7/")
  (:unix (:or "libgfortran.so.3" "libgfortran.so"))
  (t (:default "libgfortran")))

(cffi:define-foreign-library libblas
  #+:magicl.use-accelerate
  (:darwin "libBLAS.dylib" :search-path #P"/System/Library/Frameworks/Accelerate.framework/Frameworks/vecLib.framework/Versions/A/")
  #-:magicl.use-accelerate              ; KLUDGE: Look for Homebrew dir first.
  (:darwin "/usr/local/opt/lapack/lib/libblas.dylib")
  (:unix  "libblas.so")
  (t (:default "libblas")))

(cffi:define-foreign-library liblapack
  #+:magicl.use-accelerate
  (:darwin "libLAPACK.dylib" :search-path #P"/System/Library/Frameworks/Accelerate.framework/Frameworks/vecLib.framework/Versions/A/")
  #-:magicl.use-accelerate
  (:darwin "/usr/local/opt/lapack/lib/liblapack.dylib")
  (:unix  "liblapack.so")
  (t (:default "liblapack")))

(defvar *blapack-libs-loaded* nil)

(unless *blapack-libs-loaded*
  #+(or (not :darwin) (not :magicl.use-accelerate))
  (cffi:load-foreign-library 'libgfortran)
  (cffi:load-foreign-library 'libblas)
  (cffi:load-foreign-library 'liblapack)
  (setf *blapack-libs-loaded* t))

