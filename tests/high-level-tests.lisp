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

(deftest test-random-unitary ()
  "Check that we can make and identify unitaries."
  (loop :for i :from 1 :to 128 :do
    (is (magicl:unitary-matrix-p (magicl:random-unitary (list i i) :type '(complex double-float)) magicl::+double-comparison-threshold-loose+))))

(deftest test-logm ()
  "Check that the matrix logarithm is the inverse of the matrix exponential."
  (let ((x (magicl:random-unitary '(4 4) :type '(complex double-float))))
    (loop :for i :from 0 :to (1- (magicl:ncols x))
          :for j :from 0 :to (1- (magicl:nrows x))
          :do (let  ((diff (- (magicl:tref x i j)
                              (magicl:tref (magicl-transcendental:expm
                                            (magicl-transcendental:logm x))
                                           i j)))
                     (eps .00001f0))
                (is (< (abs diff) eps))))))

(deftest test-kron ()
  "Test a few properties of the kronecker product."
  (let* ((matrix-dim 2)
         (eye2 (magicl:deye 1.0 '(2 2)))
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
    (is (cl:= (magicl:nrows xxx) (magicl:ncols xxx) (expt matrix-dim 3)))
    ))


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

           (zero-p (matrix &optional (tolerance 1.0e-14))
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
                 (is (zero-p (magicl:- matrix (magicl:@ u (mul-diag-times-gen sigma vh))))))))

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
                 (is (zero-p (magicl:- matrix (magicl:@ u (mul-diag-times-gen sigma vh)))))))))

    (let ((tall-thin-matrix (magicl:rand '(8 2))))
      (check-full-svd tall-thin-matrix)
      (check-reduced-svd tall-thin-matrix))

    (let ((short-fat-matrix (magicl:rand '(2 8))))
      (check-full-svd short-fat-matrix)
      (check-reduced-svd short-fat-matrix))))
