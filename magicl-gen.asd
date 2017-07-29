(asdf:defsystem #:magicl-gen
  :depends-on (:cffi :org.middleangle.foreign-numeric-vector)
  :serial t
  :components
  ((:file "cffi-types")
   (:file "generate-interface")))
