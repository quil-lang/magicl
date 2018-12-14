;;;; transcendental/package.lisp
;;;;
;;;; Author: Robert Smith

(defpackage #:magicl-transcendental.foreign-libraries
  (:use #:common-lisp)
  (:export #:libexpokit))

(defpackage #:magicl.expokit-cffi
  (:use))

(defpackage #:magicl-transcendental
  (:use #:common-lisp
        #:cffi
        #:magicl)
  (:export #:expm #:logm))

