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
  "Check that the matrix logarithm is the inverse of the matrix exponential."
  (let ((x (magicl:random-unitary 4)))
    (loop :for i :from 0 :to (1- (magicl:matrix-cols x))
	  :for j :from 0 :to (1- (magicl:matrix-rows x))
	  :do (let  ((diff (- (magicl:ref x i j) (magicl:ref (magicl-transcendental:expm
                                                              (magicl-transcendental:logm x))
                                                             i j)))
                      (eps .00001f0))
                (is (< (abs diff) eps))))))

(deftest test-kron ()
  "Test a few properties of the kronecker product."
  (let* ((matrix-dim 2)
         (eye2 (make-identity-matrix matrix-dim))
         (eye4 (magicl:kron eye2 eye2))
         (x (magicl:make-complex-matrix matrix-dim matrix-dim '(0 1 1 0)))
         (xxx (magicl:kron x x x))
         (xxx-cols (matrix-cols xxx)))
    ;; Check that the kronecker product of two small identities is a larger identity.
    (is (magicl:identityp eye4))
    ;; Check that XXX is evaluated correctly.
    (is (loop :for i :below (matrix-rows xxx) :always
                                              (loop :for j :below xxx-cols
                                                    :always (if (= (- xxx-cols 1) (+ i j))
                                                                (= 1 (ref xxx i j))
                                                                (= 0 (ref xxx i j))))))
    ;; Check that IX /= XI.
    (is (not (equalp (magicl:kron eye2 x) (kron x eye2))))
    ;; Check that it is correctly working on more than two inputs. XXX /= XXI
    (is (not (equalp (magicl:kron x x x ) (kron x x eye2))))
    ;; Check that one of the two gives the correct analytic result.
    (is (equalp (kron x eye2) (make-complex-matrix 4 4 '(0 0 1 0
                                                         0 0 0 1
                                                         1 0 0 0
                                                         0 1 0 0))))
    ;; Check that it yields teh right dimensions.
    (is (= (matrix-rows xxx) (matrix-cols xxx) (expt matrix-dim 3)))
    ))
