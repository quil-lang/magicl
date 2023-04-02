;;;; eig.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:magicl)

;;; This file contains an experimental algorithm to compute
;;; eigenvalues of a complex matrix using a real eigenvalue solver.
;;;
;;; It is not currently compiled into MAGICL.

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
    (magicl.lapack-cffi:%dgeev          ;lapack::dgeev
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

(defun eigenvalue< (a b)
  (let ((a-real? (< (abs (imagpart a)) *junk-tol*))
        (b-real? (< (abs (imagpart b)) *junk-tol*)))
    (cond
      ((and a-real? b-real?)
       (< (realpart a) (realpart b)))
      ((and a-real? (not b-real?))
       t)
      ((and (not a-real?) b-real?)
       nil)
      (t
       ;; doesn't really matter, but we'll do something hopefully
       ;; sensible
       (or (< (abs a) (abs b))
           (< (realpart a) (realpart b))
           (< (imagpart a) (imagpart b)))))))

(defun sort-keys (keys values predicate)
  (let ((pairs (mapcar #'cons keys values)))
    (setf pairs (sort pairs predicate :key #'car))
    (values (mapcar #'car pairs)
            (mapcar #'cdr pairs))))

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

(defun double= (x y)
  (and (< (abs (- (realpart x) (realpart y))) *junk-tol*)
       (< (abs (- (imagpart x) (imagpart y))) *junk-tol*)))

(defun conjugatep (x y)
  (double= x (conjugate y)))

(defun columns (matrix)
  (loop :for i :below (ncols matrix)
        :collect (column matrix i)))

(defun organize-eigensystem (evals evecs)
  (labels ((same-vector-mod-u1 (a b)
             (assert (cl:= (nrows a) (nrows b)))
             (let ((scale nil))
               (dotimes (i (nrows a) t)
                 (cond
                   (scale
                    (unless (double= (tref a i 0)
                                     (* scale (conjugate (tref b i 0))))
                      (return-from same-vector-mod-u1 nil)))
                   ((and (double= 0.0d0 (tref a i 0))
                         (double= 0.0d0 (tref b i 0)))
                    nil)
                   ((not (or (double= 0.0d0 (tref a i 0))
                             (double= 0.0d0 (tref b i 0))))
                    (return-from same-vector-mod-u1 nil))
                   ((not (double= (abs (tref a i 0))
                                  (abs (tref b i 0))))
                    (return-from same-vector-mod-u1 nil))
                   (t
                    (setf scale (/ (tref a i 0) (conjugate (tref b i 0)))))))))
           (conjugate-pair (x y)
             (and (conjugatep (car x) (car y))
                  ;; TODO: is something like this needed?
                  ;;
                  ;; (same-vector-mod-u1 (cdr x) (cdr y))
                  )))
    (let* ((esys (cl:map 'cl:vector #'cons evals (columns evecs)))
           (n (length esys)))
      (loop :with i := 0
            :while (< i n)
            :for x := (aref esys i)
            :for buddy-pos := (position-if (lambda (y) (conjugate-pair x y)) esys :start (1+ i))
            :if buddy-pos
              :do (print 'found-byddt)
                  (rotatef (aref esys (1+ i)) (aref esys buddy-pos))
                  (incf i 2)
            :else
              :do (incf i 1))
      (values (cl:map 'list #'car esys)
              (hstack (cl:map 'list #'cdr esys))))))

(defmethod eig-lisp ((m matrix/complex-double-float))
  (assert (square-matrix-p m))
  (multiple-value-bind (evals evecs)
      (multiple-value-call #'organize-eigensystem
        (eig-lisp (embed-complex m)))
    (let ((dis-evecs (mapcar #'disembed-vector
                             (matrix-columns-as-vectors evecs))))
      (print dis-evecs)
      (assert (cl:= (length evals) (length dis-evecs)))
      (loop :until (null evals)
            :for eval := (pop evals)
            :for evec := (pop dis-evecs)
            ;; Real eigenvalues come in pairs. Just take the first
            ;; one and delete the other one.
            :if (< (abs (imagpart eval)) *junk-tol*)
              :collect eval :into final-evals
              :and :collect evec :into final-evecs
              :and :do (progn
                         (format t "~&keep/lose eval = ~A // ~A ~%" eval (pop evals))
                         (format t "keep evec = ~A~%" evec)
                         (format t "lose evec = ~A~%" (pop dis-evecs)))
            :else
              ;; Vectors that "disembed" to zero are not true
              ;; eigenvectors. Skip them. (The conjugate will be one
              ;; though.)
              :if (not (zero-vector-p evec))
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
                       (let ((evec-matrix (hstack (mapcar (lambda (vec)
                                                            (vector->column-matrix (normalize vec)))
                                                          final-evecs))))
                         (when (and (< (abs (magicl:det evec-matrix)) *junk-tol*)
                                    (not (< (abs (magicl:det m)) *junk-tol*)))
                           (error "eigenspace is singular"))
                         ;; Finally, return the purchase.
                         (return
                           (values
                            final-evals
                            evec-matrix
                            ))))))))

