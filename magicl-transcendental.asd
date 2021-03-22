;;; For backwards compatibility
;;;
;;; Please note that the MAGICL-TRANSCENDENTAL package
(asdf:defsystem #:magicl-transcendental
  :depends-on (#:magicl/ext-expokit)
  :perform (load-op :after (o c)
                    (declare (ignore o c))
                    (warn "System MAGICL-TRANSCENDENTAL is deprecated. Use or depend on system MAGICL/EXT-EXPOKIT instead."))
  :pathname "src/extensions/expokit"
  :serial t
  :components ((:file "deprecated-package")))
