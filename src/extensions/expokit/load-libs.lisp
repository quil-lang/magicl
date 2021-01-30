;;;; transcendental/load-libs.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:magicl-transcendental.foreign-libraries)

(cffi:define-foreign-library libexpokit
  (:darwin (:or #.(asdf:system-relative-pathname '#:magicl "expokit/libexpokit.dylib")
                "libexpokit.dylib"
                "expokit.dylib"))
  (:unix  (:or #.(asdf:system-relative-pathname '#:magicl "expokit/libexpokit.so")
               "libexpokit.so"
               "expokit.so"))

  (t (:default "expokit")))

(push 'libexpokit magicl.foreign-libraries::*foreign-libraries*)

(defvar *expokit-libs-loaded* nil)

(unless *expokit-libs-loaded*
  (cffi:load-foreign-library 'libexpokit)
  (setf *expokit-libs-loaded* nil))
