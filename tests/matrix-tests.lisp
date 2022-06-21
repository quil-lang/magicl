;;;; tests/matrix-tests.lisp
;;;;
;;;; Author: Cole Scott

(in-package #:magicl-tests)

(defmacro is-matrix (&rest rest)
  `(progn ,@(loop :for m :in rest
                  :collect `(is (subtypep (type-of ,m) 'matrix)))))

(defmacro is-not-matrix (&rest rest)
  `(progn ,@(loop :for m :in rest
                  :collect `(is (not (subtypep (type-of ,m) 'matrix))))))

(deftest test-identity-matrix-p ()
  "Test that identity matrices can be identified by IDENTITY-MATRIX-P for all types of matrixes from 1x1 to 64x64"
  (dolist (type +magicl-types+)
    (loop :for i :from 1 :to 64 :do
      (is (magicl:identity-matrix-p (magicl:eye (list i i) :type type)))
      (is (not (magicl:identity-matrix-p (magicl:eye (list i i) :value 2 :type type))))
      (is (not (magicl:identity-matrix-p (magicl:eye (list i i) :offset 1 :type type))))
      (is (not (magicl:identity-matrix-p (magicl:const 0 (list i i) :type type)))))))

(deftest test-square-matrix-p ()
  "Test that square matrices can be identified by IDENTITY-MATRIX-P for all types of matrixes from 1x1 to 64x64"
  (dolist (type +magicl-types+)
    (loop :for i :from 1 :to 64 :do
      (is (magicl:square-matrix-p (magicl:empty (list i i) :type type)))
      (is (not (magicl:square-matrix-p (magicl:empty (list i (* 2 i)) :type type)))))))

;; Multiplication

(deftest test-matrix-multiplication ()
  "Test multiplication for random pairs of matrices"
  (labels ((mult (a b)
             (assert (= (magicl:ncols a) (magicl:nrows b)))
             (let* ((m (magicl:nrows a))
                    (n (magicl:ncols b))
                    (p (magicl:ncols a))
                    (target (magicl:empty (list m n))))
               (loop :for i :below m :do
                 (loop :for j :below n :do
                   (setf (magicl:tref target i j)
                         (loop :for k :below p
                               :sum (* (magicl:tref a i k)
                                       (magicl:tref b k j))))))
               target)))
    (dolist (magicl::*default-tensor-type* +magicl-float-types+)
      (loop :for i :below 1000 :do
        (let* ((n (1+ (random 5)))
               (m (1+ (random 5)))
               (k (1+ (random 5)))
               (a (magicl:rand (list m k)))
               (b (magicl:rand (list k n)))
               (c (magicl:rand (list m n))))
          ;; Check that multiplication returns the correct result
          (is (magicl:=
               (mult a b)
               (magicl:mult a b)))

          ;; Check that transposing doesn't affect correctness
          (is (magicl:=
               (mult (magicl:transpose a) c)
               (magicl:mult a c :transa :t)))
          (is (magicl:=
               (mult b (magicl:transpose c))
               (magicl:mult b c :transb :t)))
          (is (magicl:=
               (mult (magicl:transpose b) (magicl:transpose a))
               (magicl:mult b a :transa :t :transb :t)))

          ;; Check that alpha correctly scales the matrices
          (is (magicl:=
               (mult (magicl:scale a 2) b)
               (magicl:mult a b :alpha (coerce 2 magicl::*default-tensor-type*)))))))))

(deftest test-matrix-vector-multiplication ()
  "Test multiplication for random pairs of matrix and vectors"
  (labels ((mult (a x)
             (assert (= (magicl:ncols a) (magicl:size x)))
             (let* ((m (magicl:nrows a))
                    (n (magicl:ncols a))
                    (target (magicl:empty (list m))))
               (loop :for i :below m :do
                 (setf (magicl:tref target i)
                       (loop :for k :below n
                             :sum (* (magicl:tref a i k)
                                     (magicl:tref x k)))))
               target)))
    (dolist (magicl::*default-tensor-type* +magicl-float-types+)
      (loop :for i :below 1000 :do
        (let* ((n (1+ (random 5)))
               (m (1+ (random 5)))
               (a (magicl:rand (list m n)))
               (x (magicl:rand (list n)))
               (y (magicl:rand (list m))))

          ;; Check that multiplication returns the correct result
          (is (magicl:=
               (mult a x)
               (magicl:mult a x)))

          ;; Check that transposing doesn't affect correctness
          (is (magicl:=
               (mult (magicl:transpose a) y)
               (magicl:mult a y :transa :t)))

          ;; Check that alpha correctly scales the matrices
          (is (magicl:=
               (mult (magicl:scale a 2) x)
               (magicl:mult a x :alpha (coerce 2 magicl::*default-tensor-type*)))))))))

(deftest test-matrix-multiplication-errors ()
  (signals simple-error (magicl:@
                         (magicl:empty '(3 3))
                         (magicl:empty '(1 1))))
  (signals simple-error (magicl:@
                         (magicl:empty '(1 2))
                         (magicl:empty '(1 2))))
  (signals simple-error (magicl:@
                         (magicl:empty '(5 2))
                         (magicl:empty '(2 3))
                         (magicl:empty '(2 3)))))

(deftest test-complex-matrix-multiplication-results ()
  "Test a few basic complex matrix multiplications"
  (let* ((m (magicl:from-list '(#C(1d0 2d0) #C(3d0 4d0) #C(5d0 6d0) #C(7d0 8d0)) '(2 2) :layout :row-major))
         (m-old (magicl::deep-copy-tensor m))
         (x (magicl:from-list '(#C(1d0 2d0) #C(3d0 4d0)) '(2 1)))
         (x-old (magicl::deep-copy-tensor x))
         (expected (magicl:from-list '(#C(-10d0 28d0) #C(-18d0 68d0)) '(2 1))))
    ;; Check that the multiplication is correct and does not throw any errors
    (is (magicl:= expected (magicl:@ m x)))

    ;; Check that the multiplication did not modify the inputs
    (is (magicl:= m-old m))
    (is (magicl:= x-old x))

    ;; Check that doing 2x1 @ 2x2 errors
    (signals error (magicl:@ x m))))

(deftest test-random-unitary ()
  "Test that random unitaries are unitary."
  (loop :repeat 100
        :for dim := (1+ (random 16))
        :for shape := (list dim dim)
        :for x := (magicl:random-unitary shape :type '(complex single-float))
        :for y := (magicl:random-unitary shape :type '(complex double-float))
        :do (is (magicl:unitary-matrix-p x))
            (is (magicl:unitary-matrix-p y))))

(deftest test-random-unitary-properties ()
  "Test calls to RANDOM-UNITARY for all float types and sizes 1x1 to 64x64 to check properties"
  (dolist (type +magicl-float-types+)
    (loop :for i :from 1 :to 64 :do
      (let ((m (magicl:random-unitary i :type type)))
        (is (> 5e-5 (abs (cl:-
                          (abs (magicl:det m))
                          1))))
        (is (magicl:=
             (magicl:eye (list i i) :type type)
             (magicl:@ m (magicl:conjugate-transpose m))
             5e-5))))))

;;; Block Matrix Routines

(deftest test-block-diagonal ()
  "Test that we can construct block diagonal matrices."
  (let ((expected (magicl:from-list '(0d0 0d0 0d0
                                      0d0 1d0 1d0
                                      0d0 1d0 1d0)
                                    '(3 3))))
    (is (magicl:= expected
                  (magicl:block-diag
                   (list (magicl:zeros '(1 1))
                         (magicl:ones '(2 2))))))))

(deftest test-matrix-stacking ()
  "Test that we can stack matrices 'horizontally' and 'vertically'."
  (let ((expected (magicl:from-list '(1 2 3
                                      4 5 6)
                                    '(2 3))))
    (is (magicl:= expected
                  (magicl:hstack
                   (loop :for j :below 3
                         :collect (magicl:column expected j)))))
    (is (magicl:= expected
                  (magicl:vstack
                   (loop :for i :below 2
                         :collect (magicl:row expected i)))))))


(deftest test-block-matrix-construction ()
  "Test that we can construct a block matrix."
  (let ((mat
          (magicl::block-matrix (list (magicl:zeros '(3 2))     (magicl:eye 3 :value 3d0)
                                      (magicl:eye 2 :value 2d0)     (magicl:zeros '(2 3)))
                                '(2 2))))
    (is (magicl:=
         mat
         (magicl:from-list '(0d0 0d0 3d0 0d0 0d0
                             0d0 0d0 0d0 3d0 0d0
                             0d0 0d0 0d0 0d0 3d0
                             2d0 0d0 0d0 0d0 0d0
                             0d0 2d0 0d0 0d0 0d0)
                           '(5 5))))))

(deftest test-doolittle ()
  (loop :for n :from 2 :to 6 :do
    (loop :repeat 100 :do
      (let ((m (magicl:rand (list n n))))
        (multiple-value-bind (l u) (magicl::doolittle m)
          (is (magicl:= m (magicl:@ l u) 1d-10)))))))
