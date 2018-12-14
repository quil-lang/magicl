;;;; magicl-transcendental.asd
;;;;
;;;; Author: Robert Smith

(asdf:defsystem #:magicl-transcendental
  :license "BSD 3-Clause (See LICENSE.txt)"
  :maintainer "Rigetti Computing"
  :version (:read-file-form "VERSION.txt")
  :depends-on (#:alexandria
               #:cffi
               #:cffi-libffi
               #:magicl)
  :in-order-to ((asdf:test-op (asdf:test-op #:magicl-tests)))
  :serial t
  :components
  ((:module "transcendental"
    :serial t
    :components ((:file "package")
                 (:file "load-libs")
                 (:file "transcendental")))
   (:file "expokit-cffi")))

