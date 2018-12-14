;;;; magical-gen.asd
;;;;
;;;; Author: Robert Smith

(asdf:defsystem #:magicl-gen
  :depends-on (#:cffi
               #:cffi-libffi)
  :serial t
  :components
  ((:file "packages")
   (:module "transcendental"
    :serial t
    :components ((:file "package")))
   (:file "cffi-types")
   (:file "generate-interface")))
