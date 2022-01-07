;;;; load-libs.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:magicl.foreign-libraries)

(cffi:define-foreign-library libexpokit
  (:darwin (:or "libexpokit.dylib" "expokit.dylib"))
  (:unix  (:or "libexpokit.so" "expokit.so"))
  (t (:default "expokit")))

(pushnew 'libexpokit *foreign-libraries*)


(pushnew (asdf:apply-output-translations "expokit/")
         cffi:*foreign-library-directories*
         :test #'equal)

;; Keep above in sync with 'perform ((... compile-op) (... f->so))'
;; method in magicl.asd.


(export 'libexpokit)

(defvar *expokit-libs-loaded* nil)

(unless *expokit-libs-loaded*
  (cffi:load-foreign-library 'libexpokit)
  (setf *expokit-libs-loaded* t))

(magicl:define-backend :expokit
  :documentation "Functions available from Expokit."
  :default t)
