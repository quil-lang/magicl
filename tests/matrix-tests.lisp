;;;; tests/matrix-tests.lisp
;;;;
;;;; Author: Cole Scott

(in-package #:magicl-tests)

(defconstant +magicl-types+
  '(single-float
    double-float
    (complex single-float)
    (complex double-float)
    (signed-byte 32)))

(defconstant +magicl-float-types+
  '(single-float
    double-float
    (complex single-float)
    (complex double-float)))

(defconstant +magicl-matrix-classes+
  '(magicl::matrix/single-float
    magicl::matrix/double-float
    magicl::matrix/complex-single-float
    magicl::matrix/complex-double-float
    magicl::matrix/int32))

(defmacro is-matrix (&rest rest)
  `(progn ,@(loop :for m :in rest
                  :collect `(is (subtypep (type-of ,m) 'matrix)))))

(defmacro is-not-matrix (&rest rest)
  `(progn ,@(loop :for m :in rest
                  :collect `(is (not (subtypep (type-of ,m) 'matrix))))))

(deftest test-identity-matrix-p ()
  "Test that identity matrices can be identified by IDENTITY-MATRIX-P for all types of matrixes from 1x1 to 64x64"
  (loop :for i :from 1 :to 64
        :do (loop :for type :in +magicl-types+
                  :do (is (identity-matrix-p (deye 1 (list i i) :type type)))
                      (is (not (identity-matrix-p (deye 2 (list i i) :type type))))
                      (is (not (identity-matrix-p (const 0 (list i i) :type type)))))))

(deftest test-square-matrix-p ()
  "Test that square matrices can be identified by IDENTITY-MATRIX-P for all types of matrixes from 1x1 to 64x64"
  (loop :for i :from 1 :to 64
        :do (loop :for type :in +magicl-types+
                  :do (is (square-matrix-p (empty (list i i) :type type)))
                      (is (not (square-matrix-p (empty (list i (* 2 i)) :type type)))))))

(deftest test-matrix-rank ()
  (is (cl:= 2 (rank (empty '(4 5))))))

;; Multiplication

(deftest test-matrix-multiplication-errors ()
  (signals simple-error (magicl:@
                  (empty '(3 3))
                  (empty '(1 1))))
  (signals simple-error (magicl:@
                  (empty '(1 2))
                  (empty '(1 2))))
  (signals simple-error (magicl:@
                  (empty '(5 2))
                  (empty '(2 3))
                  (empty '(2 3))))
  t)

(deftest test-complex-matrix-multiplication-results ()
  "Test a few basic complex matrix multiplications"
  (let* ((m-old (magicl:from-list '(#C(1d0 2d0) #C(3d0 4d0) #C(5d0 6d0) #C(7d0 8d0)) '(2 2)))
         (m (magicl:from-list '(#C(1d0 2d0) #C(3d0 4d0) #C(5d0 6d0) #C(7d0 8d0)) '(2 2) :order :row-major))
         (x-old (magicl:from-list '(#C(1d0 2d0) #C(3d0 4d0)) '(2 1)))
         (x (magicl:from-list '(#C(1d0 2d0) #C(3d0 4d0)) '(2 1)))
         (expected (magicl:from-list '(#C(-10d0 28d0) #C(-18d0 68d0)) '(2 1))))
    ;; Check that the multiplication is correct and does not throw any errors
    (is (magicl:= expected (magicl:@ m x)))

    ;; Check that the multiplication did not modify the inputs
    (is (magicl:= m-old m))
    (is (magicl:= x-old x))

    ;; Check that doing 2x1 @ 2x2 errors
    (signals error (magicl:@ x m))
    t))

(deftest test-random-unitary-properties ()
  "Test calls to RANDOM-UNITARY for all float types and sizes 1x1 to 64x64 to check properties"
  (loop :for type :in +magicl-float-types+
        :do (loop :for i :from 1 :to 64
                  :do (let ((m (magicl:random-unitary (list i i) :type type)))
                        (is (> 5e-5 (abs (cl:-
                                           (abs (magicl:det m))
                                           1))))
                        (is (magicl:=
                             (magicl:deye 1 (list i i) :type type)
                             (magicl:@ m (magicl:transpose m))
                             5e-5)))))
  t)
