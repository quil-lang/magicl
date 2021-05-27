;;;; tests/matrix-tests.lisp
;;;;
;;;; Authors: Cole Scott
;;;           Erik Davis

(in-package #:magicl-tests)

(defparameter *matrix-tests-float-types*
  '(double-float (complex double-float)))

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
    (dolist (magicl::*default-tensor-type* *matrix-tests-float-types*)
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
    (dolist (magicl::*default-tensor-type* *matrix-tests-float-types*)
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


(deftest test-kron ()
  "Test a few properties of the kronecker product."
  (let* ((matrix-dim 2)
         (eye2 (magicl:eye '(2 2)))
         (eye4 (magicl:kron eye2 eye2))
         (x (magicl:from-list '(0.0 1.0 1.0 0.0) '(2 2)))
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
    (is (not (magicl:= (magicl:kron eye2 x) (magicl:kron x eye2))))
    ;; Check that it is correctly working on more than two inputs. XXX /= XXI
    (is (not (magicl:= (magicl:kron x x x) (magicl:kron x x eye2))))
    ;; Check that one of the two gives the correct analytic result.
    (is (magicl:= (magicl:kron x eye2)
                  (magicl:from-list '(0 0 1 0
                                      0 0 0 1
                                      1 0 0 0
                                      0 1 0 0)
                                    '(4 4)
                                    :type '(complex double-float))))
    ;; Check that it yields the right dimensions.
    (is (cl:= (magicl:nrows xxx) (magicl:ncols xxx) (expt matrix-dim 3)))))

;; QR and Eigenvalue Tests

(deftest test-qr-special-cases ()
  "Test that the QR factorization works as advertised in a few special cases."
  (dolist (mat (list
		(magicl:eye 3)
		(magicl:ones '(3 1))
		(magicl:from-list '(#C(1d0 0d0) #C(0d0 1d0) #C(1d0 1d0) #C(0d0 0d0)) '(2 2))))
    (multiple-value-bind (Q R) (magicl:qr mat)
      (is (magicl:= (magicl:@ (magicl:dagger Q) Q)
		    (magicl:eye (magicl:ncols Q) :type (magicl:element-type Q))))
      (is (magicl:= R (magicl:upper-triangular R)))
      (is (magicl:= mat (magicl:@ Q R))))))


(deftest test-random-qr-factorization ()
  "Check that we can do QR factorizations for nonsquare matrices."
  (let ((repetitions 10))
    (dotimes (i repetitions)      
      (let ((tall (magicl:rand '(10 4))))
        (multiple-value-bind (a b)
            (magicl:qr tall)
          (is (magicl:= (magicl:@ a b) tall)))))))

(deftest test-hermitian-eig ()
  "Test that we can compute eigenvectors & values of Hermitian matrices."
  (let ((matrix-size 8)
        (repetitions 10))
    (dotimes (i repetitions)
      (let ((H (magicl::random-hermitian matrix-size)))
        (multiple-value-bind (evals Q) (magicl:hermitian-eig H)
          (let ((recovered (magicl:@ Q
                                     (magicl:from-diag evals :type '(complex double-float))
                                     (magicl:dagger Q))))
            (is (magicl:= H recovered 1d-12))))))
    )) ; TODO: pick this systematically


(deftest test-svd ()
  "Test the full and reduced SVDs."
  (labels ((mul-diag-times-gen (diag matrix)
             "Returns a newly allocated matrix resulting from the product of DIAG (a diagonal real matrix) with MATRIX (a complex matrix)."
             #+ignore
             (declare (type matrix diag matrix)
                      (values matrix))
             (let* ((m (magicl:nrows diag))
                    (k (magicl:ncols matrix))
                    (result (magicl:empty (list m k))))
               (dotimes (i (min m (magicl:ncols diag)) result)
                 (let ((dii (magicl:tref diag i i)))
                   (dotimes (j k)
                     (setf (magicl:tref result i j)
                           (* dii (magicl:tref matrix i j))))))))

           (norm-inf (matrix)
             "Return the infinity norm of vec(MATRIX)."
             (let ((data (magicl::storage matrix)))
               (reduce #'max data :key #'abs)))

           (zero-p (matrix &optional (tolerance (* 1.0d3 double-float-epsilon)))
             "Return T if MATRIX is close to zero (within TOLERANCE)."
             (< (norm-inf matrix) tolerance))

           (check-full-svd (matrix)
             "Validate full SVD of MATRIX."
             (let ((m (magicl:nrows matrix))
                   (n (magicl:ncols matrix)))
               (multiple-value-bind (u sigma vh)
                   (magicl:svd matrix)
                 (is (= (magicl:nrows u) (magicl:ncols u) m))
                 (is (and (= (magicl:nrows sigma) m) (= (magicl:ncols sigma) n)))
                 (is (= (magicl:nrows vh) (magicl:ncols vh) n))
                 (is (zero-p (magicl:.- matrix (magicl:@ u (mul-diag-times-gen sigma vh))))))))

           (check-reduced-svd (matrix)
             "Validate reduced SVD of MATRIX."
             (let* ((m (magicl:nrows matrix))
                    (n (magicl:ncols matrix))
                    (k (min m n)))

               (multiple-value-bind (u sigma vh)
                   (magicl:svd matrix :reduced t)
                 (is (and (= (magicl:nrows u) m)
                          (= (magicl:ncols u) k)))
                 (is (= (magicl:nrows sigma) (magicl:ncols sigma) k))
                 (is (and (= (magicl:nrows vh) k)
                          (= (magicl:ncols vh) n)))
                 (is (zero-p (magicl:.- matrix (magicl:@ u (mul-diag-times-gen sigma vh)))))))))

    (let ((tall-thin-matrix (magicl:rand '(8 2))))
      (check-full-svd tall-thin-matrix)
      (check-reduced-svd tall-thin-matrix))

    (let ((short-fat-matrix (magicl:rand '(2 8))))
      (check-full-svd short-fat-matrix)
      (check-reduced-svd short-fat-matrix))))

;;; Misc


(deftest test-determinant ()
  "Test that DET works."
  (let* ((x (magicl:from-list '(6 4 2 1 -2 8 1 5 7) '(3 3)
                              :type 'double-float))
         (d (magicl:det x)))
    (is (= d -306d0))))

(deftest test-p-norm ()
  "Test that the p-norm of vectors returns sane values."
  ;; Basic 3-4-5
  (is (= 5 (magicl:norm (magicl:from-list '(3 4) '(2)))))
  ;; One element vector should return element for all
  (loop :for val :in '(-3 0 10) :do
    (let ((x (magicl:from-list (list val) '(1))))
      (is (= (abs val) (magicl:norm x 1)))
      (is (= (abs val) (magicl:norm x 2)))
      (is (= (abs val) (magicl:norm x 3)))
      (is (= (abs val) (magicl:norm x :infinity)))))
  ;; Test known values
  (let ((x (magicl:from-list '(1 -2 3 4 5 -6) '(6))))
    (is (= 6 (magicl:norm x :infinity)))
    (is (= 21 (magicl:norm x 1)))
    (is (= 9.539392 (magicl:norm x 2)))))

(deftest test-expi ()
  "Test that we can compute exp(iH) for Hermitian (or real symmetric) H."
  (labels ((pauli-expi-theta (theta H)
             ;; for hermitian H with H^2 = I, we have
             ;; exp(itH) = cos(t)I + i sin(t)H
             (let ((H (magicl::coerce-type H '(complex double-float))))
               (magicl:.+ (magicl:eye (magicl:shape H)
                                      :value (cos theta)
                                      :type '(complex double-float))
                          (magicl:scale H (* (sin theta) #C(0d0 1d0)))))))
    (dolist (element-type '(double-float (complex double-float)))
      (let ((x (magicl:from-list '(0d0 1d0 1d0 0d0) '(2 2) :type element-type))
            (z (magicl:from-diag '(1d0 -1d0) :type element-type)))

        (dolist (theta (list 0 (/ pi 4) (/ pi 2) pi))
          (is (magicl:= (pauli-expi-theta theta x)
                        (magicl:expi (magicl:scale x theta))))
          (is (magicl:= (pauli-expi-theta theta z)
                        (magicl:expi (magicl:scale z theta)))))))))
