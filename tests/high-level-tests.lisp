;;;; tests/high-level-tests.lisp
;;;;
;;;; Author: Joseph Lin
;;;;         Robert Smith

(in-package #:magicl-tests)

(defconstant +double-float-epsilon+ #+sbcl sb-c::double-float-epsilon #-sbcl 1.1102230246251568d-16)

(deftest test-determinant ()
  "Test that DET works."
  (let* ((x (magicl::from-list (list 6 4 2 1 -2 8 1 5 7) '(3 3)
                               :type 'double-float))
         (d (magicl::det x)))
    (is (cl:= d -306d0))))

(deftest test-examples ()
  "Run all of the examples. Does not check for their correctness."
  (is (magicl-examples:all-examples)))

#+ignore
(deftest test-random-unitary ()
  "Check that we can make and identify unitaries."
  (loop :for i :from 1 :to 128 :do
    (is (magicl:unitaryp (magicl:random-unitary i)))))

#+ignore
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

#+ignore
(deftest test-kron ()
  "Test a few properties of the kronecker product."
  (let* ((matrix-dim 2)
         (eye2 (magicl:deye 1.0 '(2 2)))
         (eye4 (magicl:kron eye2 eye2))
         (x (magicl:deye 1.0 '(2 2)))
         (xxx (magicl:kron x x x))
         (xxx-cols (magicl:ncols xxx)))
    ;; Check that the kronecker product of two small identities is a larger identity.
    (is (magicl:identity-matrix-p eye4))
    ;; Check that XXX is evaluated correctly.
    (is (loop :for i :below (magicl:nrows xxx)
              :always
              (loop :for j :below xxx-cols
                    :always (if (cl:= (cl:- xxx-cols 1) (cl:+ i j))
                                (cl:= 1 (magicl:tref xxx i j))
                                (cl:= 0 (magicl:tref xxx i j))))))
    ;; Check that IX /= XI.
    (is (not (equalp (magicl:kron eye2 x) (magicl:kron x eye2))))
    ;; Check that it is correctly working on more than two inputs. XXX /= XXI
    (is (not (equalp (magicl:kron x x x ) (magicl:kron x x eye2))))
    ;; Check that one of the two gives the correct analytic result.
    (is (equalp (magicl:kron x eye2)
                (magicl:from-list '(0 0 1 0
                                    0 0 0 1
                                    1 0 0 0
                                    0 1 0 0)
                                  '(4 4)
                                  :type '(complex double-float))))
    ;; Check that it yields teh right dimensions.
    (is (cl:= (magicl:nrows xxx) (magicl:ncols xxx) (expt matrix-dim 3)))
    ))

