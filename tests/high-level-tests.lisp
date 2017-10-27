;;;; tests/high-level-tests.lisp
;;;;
;;;; Author: Joseph Lin
;;;;         Robert Smith

(in-package #:magicl-tests)

(deftest test-determinant ()
  "Test that DET works."
  (let* ((x (magicl::make-complex-matrix 3 3 (list 6 4 2 1 -2 8 1 5 7)))
         (d (magicl::det x)))
    (is (= d -306.0d0))))

(deftest test-examples ()
  "Run all of the examples. Does not check for their correctness."
  (is (magicl-examples:all-examples)))

(deftest test-identity-checking ()
  "Check that we can make and identify identities."
  (loop :for i :from 1 :to 128 :do
    (is (magicl:identityp (magicl:make-identity-matrix i)))))

(deftest test-random-unitary ()
  "Check that we can make and identify unitaries."
  (loop :for i :from 1 :to 128 :do
    (is (magicl:unitaryp (magicl:random-unitary i)))))
