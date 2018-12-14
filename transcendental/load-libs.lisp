;;;; transcendental/load-libs.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:magicl-transcendental.foreign-libraries)

(cffi:define-foreign-library libexpokit
  (:darwin (:or #.(merge-pathnames "libexpokit.dylib"
                                   (or *compile-file-truename*
                                       *load-truename*))
                "libexpokit.dylib"
                "expokit.dylib"))
  (:unix  (:or #.(merge-pathnames "libexpokit.so"
                                  (or *compile-file-truename*
                                      *load-truename*))
               "libexpokit.so"
               "expokit.so"))

  (t (:default "expokit")))

(push 'libexpkit magicl.foreign-libraries::*cffi-libraries*)

(defvar *expokit-libs-loaded* nil)

(unless *expokit-libs-loaded*
  (cffi:load-foreign-library 'libexpokit)
  (setf *expokit-libs-loaded* nil))
