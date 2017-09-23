;;;; magical-gen.asd
;;;;
;;;; Author: Robert Smith

(asdf:defsystem #:magicl-gen
  :depends-on (#:cffi
               #:cffi-libffi)
  :serial t
  :components
  ((:file "packages")
   (:file "cffi-types")
   (:file "generate-interface")))
