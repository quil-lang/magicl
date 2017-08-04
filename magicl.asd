(asdf:defsystem #:magicl
  :license "BSD 3-Clause (See LICENSE.txt)"
  :maintainer "Rigetti Computing"
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
