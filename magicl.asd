(asdf:defsystem #:magicl
  :depends-on (#:cffi #:org.middleangle.foreign-numeric-vector)
  :serial t
  :components
  ((:file "packages")
   (:file "load-libs")
   (:file "cffi-types")
   (:file "blas-cffi")
   (:file "lapack-cffi")
   (:file "magicl")))
