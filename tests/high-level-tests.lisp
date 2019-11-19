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
  "Test the complete and reduced SVDs."
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
               (reduce (lambda (x y) (max x (abs y)))
                       data :start 1 :initial-value (abs (aref data 0)))))

           (zero-p (matrix &optional (tolerance 1.0e-14))
             "Return T if MATRIX is close to zero (within TOLERANCE)."
             (< (norm-inf matrix) tolerance))

           (check-complete-svd (matrix)
             "Validate complete SVD of MATRIX."
             (let ((m (matrix-rows matrix))
                   (n (matrix-cols matrix)))
               (multiple-value-bind (u sigma vh)
                   (svd matrix)
                 (is (= (matrix-rows u) (matrix-cols u) m))
                 (is (= (matrix-rows sigma) m) (= (matrix-cols sigma) n))
                 (is (= (matrix-rows vh) (matrix-cols vh) n))
                 (is (zero-p (sub-matrix matrix (multiply-complex-matrices u (mul-diag-times-gen sigma vh))))))))

           (check-reduced-svd (matrix)
             "Validate reduced SVD of MATRIX."
             (let* ((m (matrix-rows matrix))
                    (n (matrix-cols matrix))
                    (k (min m n)))

               (multiple-value-bind (u sigma vh)
                   (svd matrix t)
                 (is (and (= (matrix-rows u) m)
                          (= (matrix-cols u) k)))
                 (is (= (matrix-rows sigma) (matrix-cols sigma) k))
                 (is (and (= (matrix-rows vh) k)
                          (= (matrix-cols vh) n)))
                 (is (zero-p (sub-matrix matrix (multiply-complex-matrices u (mul-diag-times-gen sigma vh)))))))))

    (let ((tall-thin-matrix (random-matrix 8 2)))
      (check-complete-svd tall-thin-matrix)
      (check-reduced-svd tall-thin-matrix))

    (let ((short-fat-matrix (random-matrix 2 8)))
      (check-complete-svd short-fat-matrix)
      (check-reduced-svd short-fat-matrix))))
