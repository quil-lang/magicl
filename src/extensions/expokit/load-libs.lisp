;;;; load-libs.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:magicl.foreign-libraries)

(cffi:define-foreign-library libexpokit
  (:darwin (:or #.(asdf:system-relative-pathname '#:magicl "expokit/libexpokit.dylib")
                "libexpokit.dylib"
                "expokit.dylib"))
  (:unix  (:or #.(asdf:system-relative-pathname '#:magicl "expokit/libexpokit.so")
               "libexpokit.so"
               "expokit.so"))
  (t (:default "expokit")))

(pushnew 'libexpokit *foreign-libraries*)
(export 'libexpokit)

(defvar *expokit-libs-loaded* nil)

(unless *expokit-libs-loaded*
  (cffi:load-foreign-library 'libexpokit)
  (setf *expokit-libs-loaded* nil))

(magicl:define-backend :expokit
  :documentation "Functions available from Expokit."
  :default t)
