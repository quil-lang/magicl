;;; For backwards compatibility. Do not use in new code.
(defpackage #:magicl-transcendental.**deprecated**
  (:nicknames #:magicl-transcendental)
  (:documentation "This package is deprecated. Use MAGICL instead.")
  (:use)
  (:import-from #:magicl #:expm #:logm)
  (:export #:expm #:logm))

(warn "The package MAGICL-TRANSCENDENTAL is deprecated. Use MAGICL instead.")
