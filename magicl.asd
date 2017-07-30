(asdf:defsystem #:magicl
  :depends-on (#:cffi
               #:cffi-libffi
               #:org.middleangle.foreign-numeric-vector)
  :serial t
  :components
  ((:file "packages")
   (:file "load-libs")
   (:file "cffi-types")
   (:file "blas-cffi")
   #+#:ignore (:file "lapack-cffi")
   (:file "magicl")))
