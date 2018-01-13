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

(deftest test-logm ()
  "Check that the matrix logarithm is the inverse of the matrix exponential"
  (let ((x (magicl:random-unitary 4)))
    (loop :for i :from 0 :to (1- (magicl:matrix-cols x))
	  :for j :from 0 :to (1- (magicl:matrix-rows x))
	  :do (let  ((diff (- (magicl:ref x i j) (magicl:ref (magicl:expm (magicl:logm x)) i j)))
                      (eps .00001f0))
                (is (< (abs diff) eps))))))
