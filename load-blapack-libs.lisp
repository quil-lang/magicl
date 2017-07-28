;; Copyright rif 2006.
;; Modified BSD License (see LICENSE file in this directory).

(defpackage :org.middleangle.load-blapack-libs
  (:use :common-lisp :cffi)
  (:export *blapack-libs-loaded*))

(in-package :org.middleangle.load-blapack-libs)

;; EDIT THESE VARIABLES TO POINT TO YOUR LIBRARIES!


#|
;;; The suggestion (from Leo (sdl.web at gmail dot com) to use native
;;; accelerated BLAS and LAPACK on MacOSX is to COMMENT OUT the
;;; gfortran lib DEFPARAMETER and LOAD-FOREIGN-LIBRARY, as it is not
;;; linked in, and to use the following locations:

  ;; (defparameter *gfortran-lib* "/usr/lib/libgfortran.so.3")
  (defparameter *blas-lib*
     "/System/Library/Frameworks/Accelerate.framework/Frameworks/vecLib.framework/Versions/A/libBLAS.dylib")
  (defparameter *lapack-lib*
     "/System/Library/Frameworks/Accelerate.framework/Frameworks/vecLib.framework/Versions/A/libLAPACK.dylib")

;;; and then for the load, comment it out:

+      ;; (load-foreign-library *gfortran-lib*)

;;; This might not hold for your MacOSX setup, please report back!


;;; THANKS to David Hodge, we have that included directly in the code.   Sorry that I forgot about it!! 

;;; However, I (Tony Rossini) am leaving the code above for later use.

|#


(eval-when (:compile-toplevel :load-toplevel)


#+darwin (progn
	   (defparameter *gfortran-lib* nil)
	   (defparameter *blas-lib*
	     "/System/Library/Frameworks/Accelerate.framework/Frameworks/vecLib.framework/Versions/A/libBLAS.dylib")
	   (defparameter *lapack-lib*
	     "/System/Library/Frameworks/Accelerate.framework/Frameworks/vecLib.framework/Versions/A/libLAPACK.dylib"))
#-darwin (progn
	   (defparameter *gfortran-lib* "libgfortran.so.3")
	   (defparameter *blas-lib* "libblas.so")
	   (defparameter *lapack-lib* "liblapack.so"))


  (defvar *blapack-libs-loaded* nil)

  (unless *blapack-libs-loaded*
    (progn
      (when *gfortran-lib* (load-foreign-library *gfortran-lib*))
      (load-foreign-library *blas-lib*)
      (load-foreign-library *lapack-lib*)
      (setf *blapack-libs-loaded* t))))

