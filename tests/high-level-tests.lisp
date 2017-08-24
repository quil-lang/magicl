;;;; tests/high-level-tests.lisp
;;;;
;;;; Author: Joseph Lin

(in-package #:magicl-tests)

(deftest test-determinant ()
  "Test that DET works."
  (let* ((x (magicl::make-complex-matrix 3 3 6 4 2 1 -2 8 1 5 7))
         (d (magicl::det x)))
    (is (= d -306.0d0))))
