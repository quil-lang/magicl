(defpackage #:magicl.expokit-cffi
  (:use))

(defpackage #:magicl-expokit.foreign-libraries
  (:use #:common-lisp)
  (:export #:libexpokit))

(defpackage #:magicl-expokit
  (:use #:common-lisp #:cffi))

