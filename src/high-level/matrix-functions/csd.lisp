;;;; csd.lisp
;;;;
;;;; Author: Juan M. Bello-Rivas
;;;;
;;;; Originally written as a part of the CL-QUIL package of the QUILC
;;;; project.

(in-package #:magicl)

;;; Compute the 2x2 Cosine-Sine decomposition of an unitary matrix using a
;;; modification of the algorithm in: E. S. Gawlik, Y. Nakatsukasa, and
;;; B. D. Sutton, “A Backward Stable Algorithm for Computing the CS
;;; Decomposition via the Polar Decomposition,” SIAM J. Matrix Anal. Appl.,
;;; vol. 39, no. 3, pp. 1448–1469, Jan. 2018. DOI:10.1137/18M1182747

(defun m*-diag-general (diag matrix)
  "Returns a newly allocated matrix resulting from the product of DIAG (a diagonal real matrix) with MATRIX (a complex matrix)."
  (declare (type matrix diag matrix)
           (values matrix))
  (let* ((m (nrows diag))
         (n (ncols diag))
         (k (ncols matrix))
         (result (zeros (list m k))))
    (dotimes (i (min m n) result)
      (let ((dii (tref diag i i)))
        (dotimes (j k)
          (setf (tref result i j) (* dii (tref matrix i j))))))))

(defun polar-decomposition (matrix &key (mode :unitary-hermitian))
  "Compute the polar decomposition of MATRIX (assumed to be a partial isometry). Return (VALUES Q H) where Q is unitary and H is Hermitian. If MODE is :UNITARY-HERMITIAN, then MATRIX = Q H. Alternatively, if MODE is :HERMITIAN-UNITARY, then MATRIX = H Q."
  (declare (type matrix matrix)
           (type (member :unitary-hermitian :hermitian-unitary) mode)
           (values matrix matrix))

  (multiple-value-bind (u sigma vh)
      (svd matrix :reduced t)

    (let* ((unitary (@ u vh))
           (hermitian (ecase mode
                        (:unitary-hermitian
                         (@ (conjugate-transpose vh) (m*-diag-general sigma vh)))
                        (:hermitian-unitary
                         (@ u (m*-diag-general sigma (conjugate-transpose u)))))))
      (values unitary hermitian))))

(defun csd-2x1 (a3 a4)
  "Compute the 2x1 Cosine-Sine decomposition of a unitary matrix A partitioned into blocks A1 A2 as shown below:

       ⎡ A3 ⎤        ⎡ A3 ⎤   ⎡ U1     ⎤ ⎡ -S ⎤
If A = ⎢    ⎥, then  ⎢    ⎥ = ⎢        ⎥ ⎢    ⎥ V2ᴴ,
       ⎣ A4 ⎦        ⎣ A4 ⎦   ⎣     U2 ⎦ ⎣  C ⎦

where U1, U2, and V2 are unitary and C^2 + S^2 = I. When the partition is P = Q = 1 (see documentation string for CSD below), we have

⎡ a1  A3 ⎤   ⎡ u1     ⎤ ⎡ c  -Sᵀ ⎤ ⎡ v1     ⎤H
⎢        ⎥ = ⎢        ⎥ ⎢        ⎥ ⎢        ⎥,
⎣ A2  A4 ⎦   ⎣     U2 ⎦ ⎣ S   C  ⎦ ⎣     V2 ⎦

where a1, u1, and v1 are complex numbers, c = cos θ, s = sin θ, Sᵀ = [ 0ᵀ s ], and

    ⎡ I   0 ⎤
C = ⎢       ⎥.
    ⎣ 0ᵀ  c ⎦

The function returns U1, U2, C, S, and V2H."
  (declare (type matrix a3 a4)
           (values matrix matrix matrix matrix matrix))

  (multiple-value-bind (w1 h1)
      (polar-decomposition a3 :mode ':unitary-hermitian)

    (multiple-value-bind (w2 h2)
        (polar-decomposition a4 :mode ':unitary-hermitian)

      (let ((b (.- h1 h2)) ; The sign convention ensures the eigenvalues are in the right order.
            (*double-comparison-threshold* (* 2 double-float-epsilon))) ;; Avoid errors in hermitian-eig due to too precise epsilon
        (multiple-value-bind (lambda2 v2)
            (hermitian-eig b)
          (declare (ignorable lambda2))

          (let* ((u1 (scale! (@ w1 v2) -1)) ; Absorb the negative sign in the unitary matrix.
                 (u2 (@ w2 v2))
                 (v2h (conjugate-transpose v2))
                 (s (@ v2h h1 v2))
                 (c (@ v2h h2 v2)))

            (values u1 u2 c s v2h)))))))

(defun lisp-csd-blocks (matrix p q)
  "Compute the 2x2 Cosine-Sine decomposition of MATRIX (assumed to be unitary and 2n×2n) partitioned into n×n blocks A1 A2 A3 A4 as shown below:

       ⎡ A1  A3 ⎤        ⎡ A1  A3 ⎤   ⎡ U1     ⎤ ⎡ C  -S ⎤ ⎡ V1     ⎤H
If A = ⎢        ⎥, then  ⎢        ⎥ = ⎢        ⎥ ⎢       ⎥ ⎢        ⎥,
       ⎣ A2  A4 ⎦        ⎣ A2  A4 ⎦   ⎣     U2 ⎦ ⎣ S   C ⎦ ⎣     V2 ⎦

where U1, U2, V1, and V2 are unitary and C^2 + S^2 = I. The values of P and Q determine the size of the partition of A or, in other words, the dimensions of the blocks A1, A2, A3, and A4.

When the partition is P = Q = 1, we have

⎡ a1  A3 ⎤   ⎡ u1     ⎤ ⎡ c  -Sᵀ ⎤ ⎡ v1     ⎤H
⎢        ⎥ = ⎢        ⎥ ⎢        ⎥ ⎢        ⎥,
⎣ A2  A4 ⎦   ⎣     U2 ⎦ ⎣ S   C  ⎦ ⎣     V2 ⎦

where a1, u1, and, v1 are complex numbers, c = cos θ, s = sin θ, Sᵀ = [ 0ᵀ s ], and

    ⎡ I   0 ⎤
C = ⎢       ⎥.
    ⎣ 0ᵀ  c ⎦

The function returns the matrices U1, U2, V1H, V2H, and the list of principal angles.

See also http://www.netlib.org/lapack/explore-html/de/d0d/zuncsd_8f.html."
  (declare (type matrix matrix)
           (values matrix matrix matrix matrix list))

  (let ((m (nrows matrix)))
    (assert (and (evenp m) (cl:= m (ncols matrix)))
            (matrix)
            "Invalid matrix size.")

    (assert (and (cl:= p q) (or (cl:= p 1) (cl:= p (/ m 2))))
            (p q)
            "This implementation of the CS decomposition supports equipartitions or partitions with (p, q) = (1, 1) only.")

    (let ((equipartition-p (cl:= p q (/ m 2)))
          (a1 (slice matrix '(0 0) (list p q)))
          (a2 (slice matrix (list p 0) (list m q)))
          (a3 (slice matrix (list 0 q) (list p m)))
          (a4 (slice matrix (list p q) (list m m))))

      (multiple-value-bind (u1 u2 c s v2h)
          ;; We modify Gawlik, Nakatsukasa, and Sutton's algorithm so that
          ;; the 2x2 CS decomposition is obtained from the largest 2x1 CS
          ;; decomposition (this only matters for the case p = q = 1).
          (csd-2x1 a3 a4)

        (let ((theta (loop :for i :from (- m p p) :below (- m p)
                           :for x := (realpart (tref c i i))
                           :for y := (realpart (tref s i i))
                           :collect (atan y x))))

          ;; In the case p = q = 1, we could find the entry of the 1x1 V1ᴴ
          ;; matrix by exploiting the structure of the C matrix and the S
          ;; vector shown in the documentation string. However, it turns out
          ;; that we can abuse POLAR-DECOMPOSITION to yield the correct
          ;; result, so we follow that path because it makes the code
          ;; simpler.
          (multiple-value-bind (v1h y)
              (let ((z (.+ (@ c (conjugate-transpose u1) a1)
                                  (@ s (conjugate-transpose u2) a2))))
                (polar-decomposition z :mode ':hermitian-unitary))
            (declare (ignorable y))

            (values (if equipartition-p u1 (slice u1 (list 0 (- m 2)) (list 1 (1- m))))
                    u2
                    (if equipartition-p v1h (slice v1h (list (- m 2) 0) (list (1- m) 1)))
                    v2h
                    theta)))))))

(define-backend-implementation csd-blocks :lisp 'lisp-csd-blocks)



;;; Blocks to Matrices

(defun csd-matrices-from-blocks (u1 u2 v1t v2t theta)
  "Calculates the matrices U, SIGMA, and VT of the CSD of a matrix from its intermediate representation, as calculated from LAPACK-CSD."
  (let ((p (nrows u1))
        (q (nrows v1t))
        (m (+ (nrows u1) (nrows u2)))
        (r (length theta)))
    (let ((u (direct-sum u1 u2))
          (sigma (const 0 (list m m) :type (element-type u1)))
          (vt (direct-sum v1t v2t)))
      (let ((diag11 (min p q))
            (diag12 (min p (- m q)))
            (diag21 (min (- m p) q))
            (diag22 (min (- m p) (- m q))))
        (let ((iden11 (- diag11 r))
              (iden12 (- diag12 r))
              (iden21 (- diag21 r))
              (iden22 (- diag22 r)))
          ;; Construct sigma from theta
          (loop :for i :from 0 :to (1- iden11)
                :do (setf (tref sigma i i) 1))
          (loop :for i :from iden11 :to (1- diag11)
                :do (setf (tref sigma i i) (cos (nth (- i iden11) theta))))
          (loop :for i :from 0 :to (1- iden12)
                :do (setf (tref sigma (- p 1 i) (- m 1 i)) -1))
          (loop :for i :from iden12 :to (1- diag12)
                :do (setf (tref sigma (- p 1 i) (- m 1 i))
                         (- (sin (nth (- r 1 (- i iden12)) theta)))))
          (loop :for i :from 0 :to (1- iden21)
                :do (setf (tref sigma (- m 1 i) (- q 1 i)) 1))
          (loop :for i :from iden21 :to (1- diag21)
                :do (setf (tref sigma (- m 1 i) (- q 1 i))
                         (sin (nth (- r 1 (- i iden21)) theta))))
          (loop :for i :from 0 :to (1- iden22)
                :do (setf (tref sigma (+ p i) (+ q i)) 1))
          (loop :for i :from iden22 :to (1- diag22)
                :do (setf (tref sigma (+ p i) (+ q i)) (cos (nth (- i iden22) theta))))))
      (values u sigma vt))))

;;; CSD is actually generic on the backend.

(defun csd (matrix p q)
  "Find the Cosine-Sine Decomposition of a matrix X given that it is to be partitioned with upper left block of dimension P-by-Q. Returns the CSD elements (VALUES U SIGMA VT) such that X=U*SIGMA*VT."
  (multiple-value-bind (u1 u2 v1t v2t theta) (csd-blocks matrix p q)
    (csd-matrices-from-blocks u1 u2 v1t v2t theta)))
