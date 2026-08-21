(in-package #:magicl.foreign-libraries)

;;; Shared by this file and the LAPACK one. Defined here because magicl/ext-blas
;;; is loaded before magicl/ext-lapack.

(defun environment-variable (name)
  "The value of environment variable NAME, or NIL if it is unset or empty."
  (let ((value (uiop:getenv name)))
    (and value (plusp (length value)) value)))

(defun load-library-designator (designator variable)
  "Load the foreign library named by DESIGNATOR and return its pathname.

If environment VARIABLE is set, its value replaces the library's search order
entirely, so that DESIGNATOR resolves to that path. Rebinding the designator
rather than merely loading the path alongside it keeps everything that reports on
the library -- MAGICL:PRINT-AVAILABILITY-REPORT in particular -- describing the
library actually in use, instead of NIL.

Note that this does not make symbol resolution any more precise. On SBCL a
:LIBRARY-qualified lookup still searches the whole process, so if two loaded
libraries export the same routine the first one loaded wins regardless of which
designator is asked. That is why the BLAS and LAPACK search orders are kept in
step: a mismatched pair does not fail, it silently resolves to whichever half
loaded first.

Signals an error if VARIABLE is set but the library cannot be loaded. Falling
back to the built-in search order would quietly restore the behaviour the caller
set the variable to avoid, and since the wrong backend can produce wrong numbers
rather than an obvious failure, doing that silently is the worst option."
  (let ((path (environment-variable variable)))
    (when path
      ;; DEFINE-FOREIGN-LIBRARY is a macro, so redefining a library at runtime
      ;; means evaluating one. This is CFFI's own interface for the job; there
      ;; is no functional entry point for it.
      (handler-case
          (eval `(cffi:define-foreign-library ,designator (t ,path)))
        (error (condition)
          (error "~A is set to ~S, which could not be used:~%  ~A"
                 variable path condition))))
    (handler-case
        (cffi:load-foreign-library designator)
      (error (condition)
        (if path
            (error "~A is set to ~S, which could not be loaded:~%  ~A"
                   variable path condition)
            (error condition))))
    (ignore-errors (namestring (cffi:foreign-library-pathname designator)))))

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

(defvar *blas-library-path* nil
  "Pathname of the BLAS shared library actually loaded, once known.")

(unless *blas-loaded*
  (setf *blas-library-path* (load-library-designator 'libblas "MAGICL_BLAS_PATH"))
  (setf *blas-loaded* t))

(magicl:define-backend :blas
  :documentation "Backend for BLAS functionality written in Fortran."
  :default t)
