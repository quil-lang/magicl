;;;; magical-gen.asd
;;;;
;;;; Author: Robert Smith

(asdf:defsystem #:magicl-gen
  :license "BSD 3-Clause (See LICENSE.txt)"
  :description "Generate MAGICL interfaces"
  :maintainer "Rigetti Computing"
  :author "Rigetti Computing"
  :depends-on (#:cffi
               #:cffi-libffi)
  :serial t
  :components
  ((:file "packages")
   (:module "transcendental"
    :serial t
    :components ((:file "package")))
   (:file "src/cffi-types")
   (:file "src/generate-interface/generate-interface")))
