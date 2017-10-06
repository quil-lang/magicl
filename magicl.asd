;;;; magicl.asd
;;;;
;;;; Author: Robert Smith

(asdf:defsystem #:magicl
  :license "BSD 3-Clause (See LICENSE.txt)"
  :maintainer "Rigetti Computing"
  :depends-on (#:alexandria
               #:cffi
               #:cffi-libffi)
  :in-order-to ((asdf:test-op (asdf:test-op #:magicl-tests)))
  :serial t
  :components
  (
   ;; Generic SBCL object reification hooks. This could be made into a
   ;; separate library. Stubbed out for non-SBCL implementations.
   #+#:ignore (:file "reify")
   ;; Start of the "real" MAGICL library.
   (:file "packages")
   (:file "load-libs")
   (:file "with-array-pointers")
   (:file "cffi-types")
   (:file "blas-cffi")
   (:file "lapack-cffi")
   (:file "expokit-cffi")
   (:file "high-level")
   (:file "random")
   (:file "magicl")))

#+#:ignore
(defmethod perform :after ((op load-op)
                           (system (eql (find-system ':magicl))))
  (uiop:symbol-call ':magicl.foreign-libraries
                    '#:print-availability-report
                    :show-available nil
                    :show-unavailable t))
