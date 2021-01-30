(defpackage #:magicl.expokit-cffi
  (:use)
  #-package-local-nicknames
  (:nicknames #:expokit))

(defpackage #:magicl-transcendental.foreign-libraries
  (:use #:common-lisp)
  (:export #:libexpokit))

(defpackage #:magicl-transcendental
  (:use #:common-lisp
        #:cffi)
  (:export #:expm #:logm))
