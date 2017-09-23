;;;; tests/package.lisp
;;;;
;;;; Author: Joseph Lin

(fiasco:define-test-package #:magicl-tests
  (:use #:magicl)
  
  ;; suite.lisp
  (:export
   #:run-magicl-tests))
