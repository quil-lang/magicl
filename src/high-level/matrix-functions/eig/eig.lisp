(in-package #:magicl)

(defvar *junk-tol* 1d-8)

(defgeneric to-row-major-lisp-array (m)
  (:method ((m matrix/double-float))
    (destructuring-bind (rows cols) (shape m)
      (let ((x (make-array (* rows cols) :element-type 'double-float :initial-element 0.0d0)))
        (dotimes (r rows x)
          (dotimes (c cols)
            (setf (aref x (+ c (* r cols))) (tref m r c))))))))

(defun cplx (a b)
  (if (zerop b) a (complex a b)))

(defun internal-eig (m)
  (let* ((n (isqrt (length m)))
         (eigs-real  (make-array n :element-type 'double-float :initial-element 0.0d0))
         (eigs-imag  (make-array n :element-type 'double-float :initial-element 0.0d0))
         (left-vecs  (make-array 0 :element-type 'double-float :initial-element 0.0d0))
         (right-vecs (make-array (* 2 n n) :element-type 'double-float :initial-element 0.0d0))
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
     (* 2 n)
     right-vecs                         ; right eigenvectors
     (* 2 n)
     work
     lwork
     0                                  ; info
     )
    (values eigs-real eigs-imag right-vecs)))

(defmethod eig-lisp ((m matrix/double-float))
  (assert (square-matrix-p m))
  (let* ((a (to-row-major-lisp-array m))
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
                             (complex (aref vecs (+ j (* i (* 2 n))))
                                      0.0d0)))
                     (incf j 1))
                    (complex
                     (let ((next (pop eigenvalues-left)))
                       (assert (cl:= next (conjugate e))
                           ()
                           ()
                           "Expected eigenvalues to come in conjugate pairs. Got ~A then ~A, which don't appear to be conjugates." e next))
                     (dotimes (i n)
                       (let ((re (aref vecs (+ j       (* i (* 2 n)))))
                             (im (aref vecs (+ (+ j 1) (* i (* 2 n))))))
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
    ;; map a+bi -> [a -b; b a] for all elements of M
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
             (- im-z)
             (tref embedding (1+ r-row) r-col)
             im-z
             (tref embedding (1+ r-row) (1+ r-col))
             re-z)))
    embedding))

(defmethod eig-lisp ((m matrix/complex-double-float))
  (assert (square-matrix-p m))
  (multiple-value-bind (evals evecs)
      (eig-lisp (embed-complex m))
    (let* ((tr (trace m))
           (tr-real (realpart tr))
           (tr-imag (imagpart tr))
           (known-vals   nil)
           (unknown-vals nil))
      (loop :for (e1 e2) :on evals :by #'cddr
            :do (cond
                  ((and (realp e1) (realp e2))
                   (assert (cl:= e1 e2))
                   (push e1 known-vals)
                   (decf tr-real e1))
                  ((and (complexp e1) (complexp e2))
                   (assert (cl:= e1 (conjugate e2)))
                   (push (complex (realpart e1)
                                  (abs (imagpart e1)))
                         unknown-vals)
                   (decf tr-real (realpart e1)))
                  (t
                   (error "unexpected eigenvalue pair"))))
      (format t "Re(tr) = ~A~%~
                 Im(tr) = ~A~%~
                 Known  = ~A~%~
                 UKnown = ~A~2%"
              tr-real
              tr-imag
              known-vals
              unknown-vals)
      (assert (< (abs tr-real) *junk-tol*))
      (format t "solving...~%")
      (loop :for sign :in (solve-plus-minus-sum
                           (mapcar #'imagpart unknown-vals)
                           tr-imag)
            :for unknown-val := (pop unknown-vals)
            :do (push (complex (realpart unknown-val)
                               (* sign (imagpart unknown-val)))
                      known-vals))

      (format t "Known    = ~A~%~
                 UKnown   = ~A~%~
                 Tr       = ~A~%~
                 sum(eig) = ~A~%"
              known-vals
              unknown-vals
              tr
              (reduce #'+ known-vals))
      (assert (< (abs (- tr (reduce #'+ known-vals))) *junk-tol*))
      known-vals)))


(defun solve-plus-minus-sum (a b)
  "Given a list of values A = (a1 a2 ... aN) and a value B, return a list of signs S = (s1 ... sN)---each of which is {-1, +1}---such that

    S.A = B.
"
  (labels ((rec (a s sum)
             (cond
               ((null a)
                (cond
                  ((< (abs (- b sum)) *junk-tol*)
                   (return-from solve-plus-minus-sum
                     (reverse s)))
                  ((< (abs (+ b sum)) *junk-tol*)
                   (return-from solve-plus-minus-sum
                     (reverse (mapcar #'- s))))
                  (t
                   ;; keep on truckin. return from REC and continue
                   ;; searching.
                   )))
               (t
                (let ((ai (pop a)))
                  (rec a (cons  1 s) (+ sum ai))
                  (rec a (cons -1 s) (- sum ai)))))))
    (rec a nil 0)))
