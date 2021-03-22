;;; For backwards compatibility

(asdf:defsystem #:magicl-transcendental
  :depends-on (#:magicl/ext-expokit)
  :perform (load-op :after (o c)
                    (declare (ignore o c))
                    (warn "MAGICL-TRANSCENDENTAL is deprecated. Use or depend on MAGICL/EXT-EXPOKIT instead.")))


