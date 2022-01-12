(defpackage #:magicl.blas-cffi
  (:use))

(defpackage #:magicl.blas
  (:use #:cl #:magicl)
  ;; XXX: This is kind of annoying...
  (:shadowing-import-from #:magicl
                          #:vector
                          #:=
                          #:map
                          #:trace
                          #:every
                          #:some
                          #:notevery
                          #:notany
                          #:make-array))
