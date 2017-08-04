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
   (:file "lapack-cffi")
   (:file "high-level")
   (:file "magicl")))

#+#:ignore
(defmethod perform :after ((op load-op)
                           (system (eql (find-system ':magicl))))
  (uiop:symbol-call ':magicl.foreign-libraries
                    '#:print-availability-report
                    :show-available nil
                    :show-unavailable t))
