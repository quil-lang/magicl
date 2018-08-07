;;;; transcendental/load-libs.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:magicl-transcendental.foreign-libraries)

(cffi:define-foreign-library libexpokit
  (:darwin "expokit.dylib")
  (:unix  "expokit.so")
  (t (:default "expokit")))

(push 'libexpkit magicl.foreign-libraries::*cffi-libraries*)

(defvar *expokit-libs-loaded* nil)

(unless *expokit-libs-loaded*
  (cffi:load-foreign-library 'libexpokit)
  (setf *expokit-libs-loaded* nil))
