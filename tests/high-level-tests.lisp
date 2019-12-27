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

(deftest test-svd ()
  "Test the full and reduced SVDs."
  (labels ((mul-diag-times-gen (diag matrix)
             "Returns a newly allocated matrix resulting from the product of DIAG (a diagonal real matrix) with MATRIX (a complex matrix)."
             (declare (type matrix diag matrix)
                      (values matrix))
             (let* ((m (matrix-rows diag))
                    (k (matrix-cols matrix))
                    (result (make-zero-matrix m k)))
               (dotimes (i (min m (matrix-cols diag)) result)
                 (let ((dii (ref diag i i)))
                   (dotimes (j k)
                     (setf (ref result i j) (* dii (ref matrix i j))))))))

           (norm-inf (matrix)
             "Return the infinity norm of vec(MATRIX)."
             (let ((data (magicl::matrix-data matrix)))
               (reduce #'max data :key #'abs)))

           (zero-p (matrix &optional (tolerance (* 1.0d2 magicl:+double-float-epsilon+)))
             "Return T if MATRIX is close to zero (within TOLERANCE)."
             (< (norm-inf matrix) tolerance))

           (check-full-svd (matrix)
             "Validate full SVD of MATRIX."
             (let ((m (matrix-rows matrix))
                   (n (matrix-cols matrix)))
               (multiple-value-bind (u sigma vh)
                   (svd matrix)
                 (is (= (matrix-rows u) (matrix-cols u) m))
                 (is (and (= (matrix-rows sigma) m) (= (matrix-cols sigma) n)))
                 (is (= (matrix-rows vh) (matrix-cols vh) n))
                 (is (zero-p (sub-matrix matrix (multiply-complex-matrices u (mul-diag-times-gen sigma vh))))))))

           (check-reduced-svd (matrix)
             "Validate reduced SVD of MATRIX."
             (let* ((m (matrix-rows matrix))
                    (n (matrix-cols matrix))
                    (k (min m n)))

               (multiple-value-bind (u sigma vh)
                   (svd matrix :reduced t)
                 (is (and (= (matrix-rows u) m)
                          (= (matrix-cols u) k)))
                 (is (= (matrix-rows sigma) (matrix-cols sigma) k))
                 (is (and (= (matrix-rows vh) k)
                          (= (matrix-cols vh) n)))
                 (is (zero-p (sub-matrix matrix (multiply-complex-matrices u (mul-diag-times-gen sigma vh)))))))))

    (let ((tall-thin-matrix (random-matrix 8 2)))
      (check-full-svd tall-thin-matrix)
      (check-reduced-svd tall-thin-matrix))

    (let ((short-fat-matrix (random-matrix 2 8)))
      (check-full-svd short-fat-matrix)
      (check-reduced-svd short-fat-matrix))))

(deftest test-csd-2x2-basic ()
  "Test CS decomposition of an equipartitioned 2x2 unitary matrix."
  (let ((x (magicl:random-unitary 2))
        (tol (* 1.0d2 magicl:+double-float-epsilon+)))
    (multiple-value-bind (u1 u2 v1h v2h theta)
        (magicl::csd-2x2-basic x 1 1)
      (multiple-value-bind (u1* u2* v1h* v2h* theta*)
          (lapack-csd x 1 1)
        (is (< (abs (- (ref u1 0 0) (ref u1* 0 0))) tol))
        (is (< (abs (- (ref u2 0 0) (ref u2* 0 0))) tol))
        (is (< (abs (- (ref v1h 0 0) (ref v1h* 0 0))) tol))
        (is (< (abs (- (ref v2h 0 0) (ref v2h* 0 0))) tol))
        (is (< (abs (- (first theta) (first theta*))) tol))))))

(deftest test-polynomial-solver ()
  "Test univariate polynomial solver."
  (flet ((make-random-polynomial (degree)
           (let ((c (magicl::matrix-data (magicl:random-matrix 1 degree))))
             (setf (aref c (1- degree)) (complex 1.0d0))
             (magicl::%make-polynomial :coefficients c))))
    (dotimes (i 10)
      (let* ((polynomial (make-random-polynomial 5))
             (roots (magicl::polynomial-solve polynomial)))
        (is (= (length roots) (1- (length (magicl:polynomial-coefficients polynomial)))))
        (dolist (root roots)
          (let ((refined-root (magicl:polynomial-newton-iteration polynomial root)))
            (is (< (abs (- root refined-root)) 1.0d-9))
            (is (< (abs (magicl:polynomial-eval polynomial refined-root))
                   (* 1.0d2 magicl:+double-float-epsilon+)))))))))
