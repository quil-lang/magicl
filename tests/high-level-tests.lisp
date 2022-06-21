;;;; tests/high-level-tests.lisp
;;;;
;;;; Author: Joseph Lin
;;;;         Robert Smith
;;;;         Cole Scott

(in-package #:magicl-tests)

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

(deftest test-examples ()
  "Run all of the examples. Does not check for their correctness."
  (is (magicl-examples:all-examples)))

(deftest test-random-unitary ()
  "Check that we can make and identify unitaries."
  (loop :for i :from 1 :to 128 :do
    (is (magicl:unitary-matrix-p (magicl:random-unitary i :type '(complex double-float)) +double-comparison-threshold-loose+))))

(deftest test-logm ()
  "Check that the matrix logarithm is the inverse of the matrix exponential."
  (let* ((x (magicl:random-unitary 4 :type '(complex double-float)))
         (h (magicl:scale (magicl:logm x) #C(0d0 -1d0)))
         (expih (magicl:expih h)))
    (loop :for i :from 0 :to (1- (magicl:ncols x))
          :for j :from 0 :to (1- (magicl:nrows x))
          :do (let  ((diff (- (magicl:tref x i j)
                              (magicl:tref expih i j)))
                     (eps .00001f0))
                (is (< (abs diff) eps))))))

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
                    (result (magicl:zeros (list m k))))
               (dotimes (i (min m (magicl:ncols diag)) result)
                 (let ((dii (magicl:tref diag i i)))
                   (dotimes (j k)
                     (setf (magicl:tref result i j)
                           (* dii (magicl:tref matrix i j))))))))

           (norm-inf (matrix)
             "Return the infinity norm of vec(MATRIX)."
             (let ((data (magicl::storage matrix)))
               (reduce #'max data :key #'abs)))

           (zero-p (matrix &optional (tolerance (* 1.0d2 double-float-epsilon)))
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


(deftest test-csd-2x2-basic ()
  "Test CS decomposition of an equipartitioned 2x2 unitary matrix."
  (let ((x (magicl:random-unitary 2 :type '(complex double-float)))
        (tol (* 1.0d2 double-float-epsilon)))
    (multiple-value-bind (u1 u2 v1h v2h theta)
        (magicl-lapack::csd-2x2-basic x 1 1)
      (multiple-value-bind (u1* u2* v1h* v2h* theta*)
          (magicl-lapack::csd-blocks-extension x 1 1)
        (is (< (abs (- (magicl:tref u1 0 0) (magicl:tref u1* 0 0))) tol))
        (is (< (abs (- (magicl:tref u2 0 0) (magicl:tref u2* 0 0))) tol))
        (is (< (abs (- (magicl:tref v1h 0 0) (magicl:tref v1h* 0 0))) tol))
        (is (< (abs (- (magicl:tref v2h 0 0) (magicl:tref v2h* 0 0))) tol))
        (is (< (abs (- (first theta) (first theta*))) tol))))))

(deftest test-lapack-csd-matrix-ordering ()
  "Test the CS decomposition of a 16x16 unitary matrix under both row-major and column-major orderings."
  (flet ((random-unitary (size layout)
           (let ((u (magicl:random-unitary size :type '(complex double-float))))
             (unless (eq layout (magicl::layout u))
               (magicl:transpose! u :fast t)
               (assert (eq layout (magicl::layout u))))
             u)))
    (let* ((m 16)
           (n (/ m 2)))
      (dolist (layout '(:column-major :row-major))
        (let* ((a (random-unitary m layout))
               (a1 (magicl:slice a '(0 0) (list n n)))
               (a2 (magicl:slice a (list n 0) (list m n)))
               (a3 (magicl:slice a (list 0 n) (list n m)))
               (a4 (magicl:slice a (list n n) (list m m))))
          (multiple-value-bind (u1 u2 v1h v2h theta)
              (magicl-lapack::csd-blocks-extension a n n)
            (let ((c (magicl:from-diag (mapcar #'cos theta) :type '(complex double-float)))
                  (s (magicl:from-diag (mapcar #'sin theta) :type '(complex double-float))))
              (is (magicl:= a1 (magicl:@ u1 c v1h)))
              (is (magicl:= a2 (magicl:@ u2 s v1h)))
              (is (magicl:= a3 (magicl:@ u1 (magicl:scale s -1) v2h)))
              (is (magicl:= a4 (magicl:@ u2 c v2h))))))))))

(deftest test-polynomial-solver ()
  "Test univariate polynomial solver."
  ;; Test random polynomials with favorable coefficients.
  (flet ((make-random-polynomial (degree)
           (let ((c (magicl::storage (magicl:rand (list 1 degree)
                                                  :type '(complex double-float)))))
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
                   (* 1.0d2 double-float-epsilon))))))))

  ;; Test polynomial with multiple roots.
  (let* ((polynomial (magicl:make-polynomial -4 8 -3 -2 1))
         (roots (magicl:polynomial-solve polynomial))
         (reference-roots '(#c(-2.0d0 0.0d0) #c(1.0d0 0.0d0) #c(1.0d0 0.0d0) #c(2.0d0 0.0d0)))
         (relative-error-tolerances (list (* 1.0d2 double-float-epsilon)
                                          (* 1.0d2 single-float-epsilon) ; Accuracy drops at double root.
                                          (* 1.0d2 single-float-epsilon)
                                          (* 1.0d2 double-float-epsilon))))
    (loop :for root :in roots
          :for reference-root :in reference-roots
          :for relative-error-tolerance :in relative-error-tolerances :do
            (is (< (abs (/ (- root reference-root) reference-root)) relative-error-tolerance)))))


(deftest test-rectangular-factorizations ()
  "Check that we can do various orthogonal factorizations for nonsquare matrices."
  (let ((tall (magicl:rand '(10 4)))
        (fat (magicl:rand '(4 10))))
    (dolist (factorization (list #'magicl:qr #'magicl:ql))
      (multiple-value-bind (a b)
          (funcall factorization tall)
        (is (magicl:= (magicl:@ a b) tall)))
      (signals error (funcall factorization fat)))

    (dolist (factorization (list #'magicl:rq #'magicl:lq))
      (multiple-value-bind (a b)
          (funcall factorization fat)
        (is (magicl:= (magicl:@ a b) fat)))
      (signals error (funcall factorization tall)))))

(deftest test-linear-solve ()
  "Check that we can solve a linear system."
  (let ((A (magicl:from-list '(0d0 1d0 1d0 2d0) '(2 2)))
        (b (magicl:from-list '(1d0 1d0) '(2))))
    (let ((x (magicl:linear-solve A b)))
      (is (magicl:= b (magicl:@ A x)))))

  (signals magicl::rank-deficiency-error
    (magicl:linear-solve (magicl:ones '(3 3)) (magicl:ones '(3)))))
