(defpackage #:magicl.expokit-cffi
  (:use))

(defpackage #:magicl-transcendental.foreign-libraries
  (:use #:common-lisp)
  (:export #:libexpokit))

(defpackage #:magicl-transcendental
  (:use #:common-lisp
        #:cffi))
