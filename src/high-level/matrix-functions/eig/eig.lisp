(in-package #:magicl)

(defvar *junk-tol* 1d-8)

(defgeneric to-row-major-lisp-array (m)
  (:method ((m matrix/double-float))
    (destructuring-bind (rows cols) (shape m)
      (let ((x (make-array (* rows cols) :element-type 'double-float :initial-element 0.0d0)))
        (dotimes (r rows x)
          (dotimes (c cols)
            (setf (aref x (+ c (* r cols))) (tref m r c))))))))

(defgeneric to-col-major-lisp-array (m)
  (:method ((m matrix/double-float))
    (destructuring-bind (rows cols) (shape m)
      (let ((x (make-array (* rows cols) :element-type 'double-float :initial-element 0.0d0)))
        (dotimes (c cols x)
          (dotimes (r rows)
            (setf (aref x (+ r (* c rows))) (tref m r c))))))))

(defun cplx (a b)
  (if (zerop b) a (complex a b)))

(defun internal-eig (m)
  (let* ((n (isqrt (length m)))
         (eigs-real  (make-array n :element-type 'double-float :initial-element 0.0d0))
         (eigs-imag  (make-array n :element-type 'double-float :initial-element 0.0d0))
         (left-vecs  (make-array 0 :element-type 'double-float :initial-element 0.0d0))
         (right-vecs (make-array (* n n) :element-type 'double-float :initial-element 0.0d0))
         (lwork      (* 4 n n))
         (work       (make-array lwork :element-type 'double-float :initial-element 0.0d0)))
    (lapack::dgeev
     "N"                                ; left eigenvectors
     "V"                                ; right eigenvectors
     n                                  ; order
     m                                  ; matrix (flattened)
     n                                  ; leading dimension
     eigs-real                          ; eigenvalues (real part)
     eigs-imag                          ; eigenvalues (imag part)
     left-vecs                          ; left eigenvectors (not computed)
     n
     right-vecs                         ; right eigenvectors
     n
     work
     lwork
     0                                  ; info
     )
    (values eigs-real eigs-imag right-vecs)))

(defmethod eig-lisp ((m matrix/double-float))
  (assert (square-matrix-p m))
  ;; Fortran arrays are column-major. Make sure we convert to column
  ;; major, *AND* index resulting arrays in column major afterward.
  (let* ((a (to-col-major-lisp-array m))
         (shape (shape m))
         (n (first shape)))
    (multiple-value-bind (val-re val-im vecs) (internal-eig a)
      (let ((eigenvalues (cl:map 'list #'cplx val-re val-im))
            (eigenvectors (zeros shape :type '(complex double-float))))
        (loop :with j := 0
              :with eigenvalues-left := eigenvalues
              :until (null eigenvalues-left)
              :for e := (pop eigenvalues-left)
              :do (etypecase e
                    (real
                     (dotimes (i n)
                       (setf (tref eigenvectors i j)
                             (complex (aref vecs (+ i (* j n)))
                                      0.0d0)))
                     (incf j 1))
                    (complex
                     (let ((next (pop eigenvalues-left)))
                       (assert (cl:= next (conjugate e))
                           ()
                           ()
                           "Expected eigenvalues to come in conjugate pairs. Got ~A then ~A, which don't appear to be conjugates." e next))
                     (dotimes (i n)
                       (let ((re (aref vecs (+ i (* j n))))
                             (im (aref vecs (+ i (* (+ j 1) n)))))
                         (setf (tref eigenvectors i j)
                               (complex re im)
                               (tref eigenvectors i (+ j 1))
                               (complex re (- im)))))
                     (incf j 2))))
        (values eigenvalues
                eigenvectors)))))

(defun embed-complex (m)
  (assert (square-matrix-p m))
  (let* ((n (nrows m))
         (embedding (zeros (list (* 2 n) (* 2 n)) :type 'double-float)))
    ;; map a+bi -> [a b; -b a] for all elements of M
    (uiop:nest
     (dotimes (z-row n))
     (let ((r-row (* 2 z-row))))
     (dotimes (z-col n))
     (let ((r-col (* 2 z-col))))
     (let* ((z (tref m z-row z-col))
            (re-z (realpart z))
            (im-z (imagpart z))))
     (progn
       (setf (tref embedding r-row      r-col)
             re-z
             (tref embedding r-row      (1+ r-col))
             im-z
             (tref embedding (1+ r-row) r-col)
             (- im-z)
             (tref embedding (1+ r-row) (1+ r-col))
             re-z)))
    embedding))

(defun matrix-columns-as-vectors (m)
  (loop :for col :below (ncols m)
        :collect (column-matrix->vector (column m col))))

(defun disembed-vector (e)
  (check-type e vector)
  (assert (evenp (size e)))
  (let* ((n (size e))
         (n/2 (/ n 2))
         (vec (zeros (list n/2)
                     :type '(complex double-float))))
    (dotimes (i n/2 vec)
      (setf (tref vec i)
            ;; TODO: Why -?
            (- (tref e (+ 0 (* 2 i)))
               (* #C(0.0d0 1.0d0)
                  (tref e (+ 1 (* 2 i)))))))))

(defun zero-vector-p (x)
  (< (norm x) *junk-tol*))

(defmethod eig-lisp ((m matrix/complex-double-float))
  (assert (square-matrix-p m))
  (multiple-value-bind (evals evecs)
      (eig-lisp (embed-complex m))
    (let ((dis-evecs (mapcar #'disembed-vector
                             (matrix-columns-as-vectors evecs))))
      (assert (cl:= (length evals) (length dis-evecs)))
      (loop :for eval :in evals
            :for evec :in dis-evecs
            ;; Vectors that "disembed" to zero are not true
            ;; eigenvectors.
            :unless (zero-vector-p evec)
              :collect eval :into final-evals
              :and :collect evec :into final-evecs
            :finally (progn
                       ;; Check for the correct number of eigenvalues
                       ;; and eigenvectors.
                       (assert (cl:= (ncols m)
                                     (length final-evals)))
                       (assert (cl:= (ncols m)
                                     (length final-evecs)))
                       ;; Check for the correct dimension of
                       ;; eigenvectors,
                       (dolist (evec final-evecs)
                         (assert (cl:= (nrows m)
                                       (size evec))))
                       ;; Extreme sanity check to verify each
                       ;; eigenvalue and eigenvector are actually such
                       ;; by definition.
                       (loop :for eval :in final-evals
                             :for evec :in final-evecs
                             :for ecol := (vector->column-matrix evec)
                             :for m*v := (magicl:@ m ecol)
                             :for l*v := (scale ecol eval)
                             :for zero := (column-matrix->vector (.- m*v l*v))
                             :do (assert (zero-vector-p zero) () ()
                                         "Got an eigenvalue L and an eigenvector V such that L*V /= M.V"))
                       ;; Finally, return the purchase.
                       (return
                         (values
                          final-evals
                          (hstack (mapcar #'vector->column-matrix final-evecs)))))))))

