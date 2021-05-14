;;;; tests/high-level-tests.lisp
;;;;
;;;; Author: Joseph Lin
;;;;         Robert Smith
;;;;         Cole Scott

(in-package #:magicl-tests)

(deftest test-examples ()
  "Run all of the examples. Does not check for their correctness."
  (is (magicl-examples:all-examples)))

(deftest test-random-unitary ()
  "Check that we can make and identify unitaries."
  (loop :for i :from 1 :to 128 :do
    (is (magicl:unitary-matrix-p (magicl:random-unitary (list i i) :type '(complex double-float)) +double-comparison-threshold-loose+))))

(deftest test-logm ()
  "Check that the matrix logarithm is the inverse of the matrix exponential."
  (let ((x (magicl:random-unitary '(4 4) :type '(complex double-float))))
    (loop :for i :from 0 :to (1- (magicl:ncols x))
          :for j :from 0 :to (1- (magicl:nrows x))
          :do (let  ((diff (- (magicl:tref x i j)
                              (magicl:tref (magicl:expm
                                            (magicl:logm x))
                                           i j)))
                     (eps .00001f0))
                (is (< (abs diff) eps))))))

(deftest test-csd-2x2-basic ()
  "Test CS decomposition of an equipartitioned 2x2 unitary matrix."
  (let ((x (magicl:random-unitary '(2 2)
                                  :type '(complex double-float)))
        (tol (* 1.0d2 double-float-epsilon)))
    (multiple-value-bind (u1 u2 v1h v2h theta)
        (magicl-lapack::csd-2x2-basic x 1 1)
      (multiple-value-bind (u1* u2* v1h* v2h* theta*)
          (magicl-lapack:lapack-csd x 1 1)
        (is (< (abs (- (magicl:tref u1 0 0) (magicl:tref u1* 0 0))) tol))
        (is (< (abs (- (magicl:tref u2 0 0) (magicl:tref u2* 0 0))) tol))
        (is (< (abs (- (magicl:tref v1h 0 0) (magicl:tref v1h* 0 0))) tol))
        (is (< (abs (- (magicl:tref v2h 0 0) (magicl:tref v2h* 0 0))) tol))
        (is (< (abs (- (first theta) (first theta*))) tol))))))

(deftest test-lapack-csd-matrix-ordering ()
  "Test the CS decomposition of a 16x16 unitary matrix under both row-major and column-major orderings."
  (flet ((random-unitary (size layout)
           (let ((u (magicl:random-unitary (list size size) :type '(complex double-float))))
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
              (magicl-lapack:lapack-csd a n n)
            (let ((c (magicl:from-diag (mapcar #'cos theta) :type '(complex double-float)))
                  (s (magicl:from-diag (mapcar #'sin theta) :type '(complex double-float))))
              (is (magicl:= a1 (magicl:@ u1 c v1h)))
              (is (magicl:= a2 (magicl:@ u2 s v1h)))
              (is (magicl:= a3 (magicl:@ u1 (magicl:scale s -1) v2h)))
              (is (magicl:= a4 (magicl:@ u2 c v2h))))))))))

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

(deftest test-random-unitary-properties ()
  "Test calls to RANDOM-UNITARY for all float types and sizes 1x1 to 64x64 to check properties"
  (dolist (type +magicl-float-types+)
    (loop :for i :from 1 :to 64 :do
      (let ((m (magicl:random-unitary (list i i) :type type)))
        (is (> 5e-5 (abs (cl:-
                          (abs (magicl:det m))
                          1))))
        (is (magicl:=
             (magicl:eye (list i i) :type type)
             (magicl:@ m (magicl:transpose m))
             5e-5))))))
