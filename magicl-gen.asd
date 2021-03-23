;;;; magical-gen.asd
;;;;
;;;; Author: Robert Smith

(asdf:defsystem #:magicl-gen
  :license "BSD 3-Clause (See LICENSE.txt)"
  :description "Generate MAGICL interfaces"
  :maintainer "Rigetti Computing"
  :author "Rigetti Computing"
  :depends-on (#:abstract-classes
               #:cffi
               #:cffi-libffi
               #:magicl/ext)
  :serial t
  :components
  ((:file "src/extensions/common/package")
   (:file "src/packages")
   (:file "src/extensions/blas/package")
   (:file "src/extensions/lapack/package")
   (:file "src/extensions/expokit/package")
   (:file "src/extensions/common/cffi-types")
   (:file "src/generate-interface/generate-interface")))
