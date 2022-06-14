(defpackage #:magicl.lapack-cffi
  (:use))

(defpackage #:magicl-lapack
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
                          #:make-array)
  (:export
   #:lapack-eig
   #:lapack-lu
   #:lapack-csd
   #:lapack-svd
   #:lapack-ql
   #:lapack-qr
   #:lapack-rq
   #:lapack-lq
   #:lapack-ql-q
   #:lapack-qr-q
   #:lapack-rq-q
   #:lapack-lq-q))
