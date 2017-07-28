;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;; Copyright rif 2006.
;; Modified BSD License (see LICENSE file in this directory).

(defsystem :org.middleangle.cl-blapack-gen
    :depends-on (:cffi :org.middleangle.foreign-numeric-vector)
    :components
    ((:file "blapack-cffi-types")
     (:file "generate-blapack-interface" :depends-on ("blapack-cffi-types"))))
