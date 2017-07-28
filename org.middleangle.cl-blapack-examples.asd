;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;; Copyright rif 2006.
;; Modified BSD License (see LICENSE file in this directory).

(defsystem :org.middleangle.cl-blapack-examples
  :depends-on (:org.middleangle.cl-blapack)
  :components
  ((:file "examples")))
