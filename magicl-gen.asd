(asdf:defsystem #:magicl-gen
  :depends-on (#:cffi
               #:cffi-libffi
               #:org.middleangle.foreign-numeric-vector)
  :serial t
  :components
  ((:file "packages")
   (:file "cffi-types")
   (:file "generate-interface")))
