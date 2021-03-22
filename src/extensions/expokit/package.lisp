(defpackage #:magicl.expokit-cffi
  (:use))

(defpackage #:magicl-expokit.foreign-libraries
  (:use #:common-lisp)
  (:export #:libexpokit))

(defpackage #:magicl-expokit
  (:use #:common-lisp
        #:cffi)
  )

;;; For backwards compatibility.
(defpackage #:magicl-transcendental.**deprecated**
  (:nicknames #:magicl-transcendental)
  (:documentation "This package is deprecated. Use MAGICL instead.")
  (:use)
  (:import-from #:magicl #:expm #:logm)
  (:export #:expm #:logm))
