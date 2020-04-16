;;;; lapack-templates.lisp
;;;;
;;;; Author: Cole Scott

;;; NOTE: This file is emulating C++-style templates to generate
;;;       methods to call LAPACK functions. This is done to avoid
;;;       writing the same method four times but results in
;;;       less-than-perfect use of macros.

(in-package #:magicl)

(defun generate-lapack-mult-for-type (matrix-class vector-class type matrix-matrix-function matrix-vector-function vector-vector-function)
  `(progn
     (defmethod mult ((a ,matrix-class) (b ,matrix-class) &key target (alpha ,(coerce 1 type)) (beta ,(coerce 0 type)) (transa :n) (transb :n))
       (policy-cond:with-expectations (> speed safety)
           ((type (member nil :n :t :c) transa)
            (type (member nil :n :t :c) transb))
         (let* ((m (if (eq :n transa) (nrows a) (ncols a)))
                (k (if (eq :n transa) (ncols a) (nrows a)))
                (n (if (eq :n transb) (ncols b) (nrows b)))
                (brows (if (eq :n transb) (nrows b) (ncols b))))
           (policy-cond:with-expectations (> speed safety)
               ((assertion (cl:= k brows))
                (assertion (or (not target) (equal (shape target) (list m n)))))
             (let ((ta
                     (if (eql :row-major (layout a))
                         (case transa
                           (:n :t)
                           (:t :n)
                           (:c (error "Specifying TRANSA to be :C is not allowed if A is ROW-MAJOR")))
                         transa))
                   (tb (if (eql :row-major (layout b))
                           (case transb
                             (:n :t)
                             (:t :n)
                             (:c (error "Specifying TRANSB to be :C is not allowed if B is ROW-MAJOR")))
                           transb))
                   (target (or target
                               (empty
                                (list m n)
                                :type ',type))))
               (,matrix-matrix-function
                (ecase ta
                  (:t "T")
                  (:c "C")
                  (:n "N"))
                (ecase tb
                  (:t "T")
                  (:c "C")
                  (:n "N"))
                m
                n
                k
                alpha
                (storage a)
                (if (eql :n ta) m k)
                (storage b)
                (if (eql :n tb) k n)
                beta
                (storage target)
                m)
               target)))))

     (defmethod mult ((a ,vector-class) (b ,matrix-class) &key target (alpha ,(coerce 0 type)) (beta ,(coerce 1 type)) transa (transb :n))
       (policy-cond:with-expectations (> speed safety)
           ((type (member nil :n :t :c) transb)
            (assertion (null transa)))
           (let ((ta (ecase transb
                       (:n :t)
                       (:t :n)
                       (:c (error "Specifying TRANSA to be :C is not supported for vector-matrix multiplication")))))
             (mult b a :target target :alpha beta :beta alpha :transa ta :transb transa))))
     
     (defmethod mult ((a ,matrix-class) (x ,vector-class) &key target (alpha ,(coerce 1 type)) (beta ,(coerce 0 type)) (transa :n) transb)
       (policy-cond:with-expectations (> speed safety)
           ((type (member nil :n :t :c) transa)
            (assertion (null transb)))
         (let* ((m-op (if (eq :n transa) (nrows a) (ncols a)))
                (n-op (if (eq :n transa) (ncols a) (nrows a))))
           (policy-cond:with-expectations (> speed safety)
               ((assertion (cl:= n-op (size x)))
                (assertion (or (not target) (equal (shape target) (list m-op)))))
             (let ((ta
                     (if (eql :row-major (layout a))
                         (case transa
                           (:n :t)
                           (:t :n)
                           (:c (error "Specifying TRANS to be :C is not allowed if A is ROW-MAJOR")))
                         transa))
                   (target (or target
                               (empty
                                (list m-op)
                                :type ',type))))
               (,matrix-vector-function
                (ecase ta
                  (:t "T")
                  (:c "C")
                  (:n "N"))
                (if (eql :column-major (layout a)) (nrows a) (ncols a))
                (if (eql :column-major (layout a)) (ncols a) (nrows a))
                alpha
                (storage a)
                (if (eql :column-major (layout a)) (nrows a) (ncols a))
                (storage x)
                1 ;; NOTE: This corresponds to the stride of X
                beta
                (storage target)
                1 ;; NOTE: This corresponds to the stride of TARGET
                )
               target)))))

     (defmethod mult ((a ,vector-class) (b ,vector-class) &key target (alpha ,(coerce 1 type)) (beta ,(coerce 0 type)) transa transb)
       (let ((n (vector-size a)))
         (policy-cond:with-expectations (> speed safety)
             ((type (member nil :n) transa)
              (type (member nil :n) transb)
              (type null target)
              (assertion (cl:= (vector-size b) n))
              (assertion (cl:= alpha ,(coerce 1 type)))
              (assertion (cl:= beta ,(coerce 0 type))))
           ;; !!! Most of the BLAS ?DOT? routines are broken on OSX
           ,(if (eq vector-vector-function 'magicl.blas-cffi:%ddot)
              `(,vector-vector-function
                n
                (storage a)
                1 ;; NOTE: This corresponds to the stride of A
                (storage b)
                1 ;; NOTE: This corresponds to the stride of B
                )
              `(magicl:dot a b)))))))

(defun generate-lapack-lu-for-type (class type lu-function)
  (declare (ignore type))
  `(progn
     (defmethod lu ((m ,class))
       (lapack-lu m))

     (defmethod lapack-lu ((a ,class))
       (let* ((a-tensor (deep-copy-tensor a))
              (a (storage a-tensor))
              (m (nrows a-tensor))
              (n (ncols a-tensor))
              (lda m)
              (ipiv-tensor (empty (list (max m n)) :type '(signed-byte 32)))
              (ipiv (storage ipiv-tensor))
              (info 0))
         (when (eql :row-major (layout a-tensor)) (transpose! a-tensor))
         (,lu-function
          m
          n
          a
          lda
          ipiv
          info)
         (values a-tensor ipiv-tensor)))))

(defun generate-lapack-inv-for-type (class type lu-function inv-function)
  `(progn
     (defmethod inv ((m ,class))
       (lapack-inv m))
     
     (defmethod lapack-inv ((a ,class))
       (let ((a-tensor (deep-copy-tensor a)))
         (when (eql :row-major (layout a-tensor)) (transpose! a-tensor))
         (let* ((a (storage a-tensor))
                (m (nrows a-tensor))
                (n (ncols a-tensor))
                (lda m)
                (ipiv-tensor (empty (list (max m n)) :type '(signed-byte 32)))
                (ipiv (storage ipiv-tensor))
                (info 0))
           (,lu-function
            m
            n
            a
            lda
            ipiv
            info)
           ;; TODO: This check is already performed by the LU
           ;;       function, however INFO is not being returned
           ;;       correctly. When the bindings are fixed this should
           ;;       just check if INFO is non-zero
           (assert (cl:notany
                    (lambda (x)
                      (cl:= x 0))
                    (diag a-tensor))
                   () "The provided matrix is singular and cannot be inverted.")
           (let* ((lwork -1)
                  (work1 (make-array (max 1 lwork) :element-type ',type))
                  (work nil)
                  (info 0))
             ;; Perform work size query with work of length 1
             (,inv-function
              n
              a
              m
              ipiv
              work1
              lwork
              info)
             (setf lwork (round (realpart (aref work1 0))))
             (setf work (make-array (max 1 lwork) :element-type ',type))
             ;; Perform actual operation with correct work size
             (,inv-function
              n
              a
              m
              ipiv
              work
              lwork
              info))
           (values a-tensor))))))

(defun generate-lapack-svd-for-type (class type svd-function &optional real-type)
  `(progn
     (defmethod svd ((m ,class) &key reduced)
       (lapack-svd m :reduced reduced))
     
     (defmethod lapack-svd ((m ,class) &key reduced)
       "Find the SVD of a matrix M. Return (VALUES U SIGMA Vt) where M = U*SIGMA*Vt. If REDUCED is non-NIL, return the reduced SVD (where either U or V are just partial isometries and not necessarily unitary matrices)."
       (let* ((jobu (if reduced "S" "A"))
              (jobvt (if reduced "S" "A"))
              (rows (nrows m))
              (cols (ncols m))
              (a (alexandria:copy-array (storage (if (eql :row-major (layout m)) (transpose m) m))))
              (lwork -1)
              (info 0)
              (k (min rows cols))
              (u-cols (if reduced k rows))
              (vt-rows (if reduced k cols))
              (lda rows)
              (s (make-array (min rows cols) :element-type ',(or real-type type)))
              (ldu rows)
              (ldvt vt-rows)
              (work1 (make-array (max 1 lwork) :element-type ',type))
              (work nil)
              ,@(when real-type
                  `((rwork (make-array (* 5 (min rows cols)) :element-type ',real-type)))))
         (let ((u (make-array (* ldu rows) :element-type ',type))
               (vt (make-array (* ldvt cols) :element-type ',type)))
           ;; run it once as a workspace query
           (,svd-function jobu jobvt rows cols a lda s u ldu vt ldvt
                          work1 lwork ,@(when real-type `(rwork)) info)
           (setf lwork (round (realpart (aref work1 0))))
           (setf work (make-array (max 1 lwork) :element-type ',type))
           ;; run it again with optimal workspace size
           (,svd-function jobu jobvt rows cols a lda s u ldu vt ldvt
                          work lwork ,@(when real-type `(rwork)) info)
           (let ((smat (make-array (* u-cols vt-rows) :element-type ',(or real-type type))))
             (dotimes (i k)
               (setf (aref smat (matrix-column-major-index i i u-cols vt-rows))
                     (aref s i)))
             (values (from-array u (list rows u-cols) :layout :column-major)
                     (from-array smat (list u-cols vt-rows) :layout :column-major)
                     (from-array vt (list vt-rows cols) :layout :column-major))))))))

;; TODO: This returns only the real parts when with non-complex numbers. Should do something different?
(defun generate-lapack-eig-for-type (class type eig-function &optional real-type)
  `(progn
     (defmethod eig ((m ,class))
       (lapack-eig m))

     (defmethod lapack-eig ((m ,class))
       (policy-cond:with-expectations (> speed safety)
           ((assertion (square-matrix-p m)))
         (let ((rows (nrows m))
               (cols (ncols m))
               (a-tensor (deep-copy-tensor m)))
           (when (eql :row-major (layout m)) (transpose! a-tensor))
           (let ((jobvl "N")
                 (jobvr "V")
                 (a (storage a-tensor))
                 ,@(if real-type
                       `((w (make-array rows :element-type ',type)))
                       `((wr (make-array rows :element-type ',type))
                         (wi (make-array rows :element-type ',type))))
                 (vl (make-array rows :element-type ',type))
                 (vr (make-array (* rows rows) :element-type ',type))
                 (lwork -1)
                 (info 0)
                 ,@(when real-type
                     `((rwork (make-array (* 2 rows) :element-type ',real-type)))))
             (let ((work (make-array (max 1 lwork) :element-type ',type)))
               ;; run it once as a workspace query
               (,eig-function jobvl jobvr rows a rows ,@(if real-type `(w) `(wr wi))
                              vl 1 vr rows work lwork ,@(when real-type `(rwork)) info)
               (setf lwork (truncate (realpart (row-major-aref work 0))))
               (setf work (make-array (max 1 lwork) :element-type ',type))
               ;; run it again with optimal workspace size
               (,eig-function jobvl jobvr rows a rows ,@(if real-type `(w) `(wr wi))
                              vl 1 vr rows work lwork ,@(when real-type `(rwork)) info)
               (values (coerce ,@(if real-type `(w) `(wr)) 'list) (from-array vr (list rows cols) :layout :column-major)))))))))

(defun generate-lapack-hermitian-eig-for-type (class type eig-function real-type)
  `(progn
     (defmethod hermitian-eig ((m ,class))
       (lapack-hermitian-eig m))

     (defmethod lapack-hermitian-eig ((m ,class))
       (policy-cond:with-expectations (> speed safety)
           ((assertion (square-matrix-p m))
            (assertion (hermitian-matrix-p m)))
         (let ((rows (nrows m))
               (a-tensor (deep-copy-tensor m)))
           (when (eql :row-major (layout m)) (transpose! a-tensor))
           (let ((jobz "V")
                 (uplo "U")
                 (n rows)
                 (a (storage a-tensor))
                 (lda rows)
                 (w (make-array rows :element-type ',real-type))
                 (work (make-array 1 :element-type ',type))
                 (lwork -1)
                 (rwork (make-array (- (* 3 rows) 2) :element-type ',real-type))
                 (info 0))
             ;; run it once as a workspace query
             (,eig-function jobz uplo n a lda w work lwork rwork info)
             (setf lwork (truncate (realpart (row-major-aref work 0))))
             (setf work (make-array (max 1 lwork) :element-type ',type))
             ;; run it again with optimal workspace size
             (,eig-function jobz uplo n a lda w work lwork rwork info)
             (values (coerce w 'list) a-tensor)))))))

;; TODO: implement row-major checks in these functions
(defun generate-lapack-ql-qr-rq-lq-for-type (class type
                                             ql-function qr-function rq-function lq-function
                                             ql-q-function qr-q-function rq-q-function lq-q-function)
  `(progn
     (defmethod qr ((m ,class))
       (policy-cond:with-expectations (> speed safety)
           ((assertion (square-matrix-p m)))
         (let ((rows (nrows m))
               (cols (ncols m)))
           (multiple-value-bind (a tau) (lapack-qr m)
             (let* ((r (upper-triangular a cols))
                    (q (lapack-qr-q a tau)))
               ;; change signs if diagonal elements of r are negative
               (dotimes (j cols)
                 (let ((diag-elt (tref r j j)))
                   (assert (zerop (imagpart diag-elt))
                           () "Diagonal element R_~D~D=~A is not real" j j diag-elt)
                   (setf diag-elt (realpart diag-elt))
                   (when (minusp diag-elt)
                     (dotimes (i rows)
                       (when (<= j i (1- cols))
                         (setf (tref r j i) (- (tref r j i))))
                       (setf (tref q i j) (- (tref q i j)))))))
               (values q r))))))
     
     (defmethod ql ((m ,class))
       (policy-cond:with-expectations (> speed safety)
           ((assertion (square-matrix-p m)))
         (let ((rows (nrows m))
               (cols (ncols m)))
           (multiple-value-bind (a tau) (lapack-ql m)
             (let* ((l (lower-triangular a cols))
                    (q (lapack-ql-q a tau)))
               ;; change signs if diagonal elements of l are negative
               (dotimes (j cols)
                 (let ((diag-elt (tref l j j)))
                   (assert (zerop (imagpart diag-elt))
                           () "Diagonal element L_~D~D=~A is not real" j j diag-elt)
                   (setf diag-elt (realpart diag-elt))
                   (when (minusp diag-elt)
                     (dotimes (i rows)
                       (when (<= i j)
                         (setf (tref l j i) (- (tref l j i))))
                       (setf (tref q i j) (- (tref q i j)))))))
               (values q l))))))

     (defmethod rq ((m ,class))
       (policy-cond:with-expectations (> speed safety)
           ((assertion (square-matrix-p m)))
         (let ((rows (nrows m))
               (cols (ncols m)))
           (multiple-value-bind (a tau) (lapack-rq m)
             (let* ((r (upper-triangular a rows))
                    (q (lapack-rq-q a tau)))
               ;; change signs if diagonal elements of r are negative
               (dotimes (i rows)
                 (let ((diag-elt (tref r i i)))
                   (assert (zerop (imagpart diag-elt))
                           () "Diagonal element R_~D~D=~A is not real" i i diag-elt)
                   (setf diag-elt (realpart diag-elt))
                   (when (minusp diag-elt)
                     (dotimes (j cols)
                       (when (<= j i)
                         (setf (tref r j i) (- (tref r j i))))
                       (setf (tref q i j) (- (tref q i j)))))))
               (values q r))))))

     (defmethod lq ((m ,class))
       (policy-cond:with-expectations (> speed safety)
           ((assertion (square-matrix-p m)))
         (let ((rows (nrows m))
               (cols (ncols m)))
           (multiple-value-bind (a tau) (lapack-lq m)
             (let* ((l (lower-triangular a rows))
                    (q (lapack-lq-q a tau)))
               ;; change signs if diagonal elements of l are negative
               (dotimes (i rows)
                 (let ((diag-elt (tref l i i)))
                   (assert (zerop (imagpart diag-elt))
                           () "Diagonal element L_~D~D=~A is not real" i i diag-elt)
                   (setf diag-elt (realpart diag-elt))
                   (when (minusp diag-elt)
                     (dotimes (j cols)
                       (when (<= i j (1- rows))
                         (setf (tref l j i) (- (tref l j i))))
                       (setf (tref q i j) (- (tref q i j)))))))
               (values q l))))))

     (defmethod lapack-qr ((m ,class))
       (let* ((rows (nrows m))
              (cols (ncols m))
              (a-tensor (deep-copy-tensor m))
              (a (storage a-tensor))
              (lwork -1)
              (info 0))
         (when (eql :row-major (layout m))
           (transpose! a-tensor))
         (let ((lda rows)
               (tau (make-array (min rows cols) :element-type ',type))
               (work (make-array (max 1 lwork) :element-type ',type)))
           ;; run it once as a workspace query
           (,qr-function rows cols a lda tau work lwork info)
           (setf lwork (round (realpart (aref work 0))))
           (setf work (make-array (max 1 lwork) :element-type ',type))
           ;; run it again with optimal workspace size
           (,qr-function rows cols a lda tau work lwork info)
           (values a-tensor
                   (from-array tau (list (min rows cols))
                               :type ',type
                               :layout :column-major)))))
     
     (defmethod lapack-ql ((m ,class))
       (let* ((rows (nrows m))
              (cols (ncols m))
              (a-tensor (deep-copy-tensor m))
              (a (storage a-tensor))
              (lwork -1)
              (info 0))
         (when (eql :row-major (layout m))
           (transpose! a-tensor))
         (let ((lda rows)
               (tau (make-array (min rows cols) :element-type ',type))
               (work (make-array (max 1 lwork) :element-type ',type)))
           ;; run it once as a workspace query
           (,ql-function rows cols a lda tau work lwork info)
           (setf lwork (round (realpart (aref work 0))))
           (setf work (make-array (max 1 lwork) :element-type ',type))
           ;; run it again with optimal workspace size
           (,ql-function rows cols a lda tau work lwork info)
           (values a-tensor
                   (from-array tau (list (min rows cols))
                               :type ',type
                               :layout :column-major)))))

     (defmethod lapack-rq ((m ,class))
       (let* ((rows (nrows m))
              (cols (ncols m))
              (a-tensor (deep-copy-tensor m))
              (a (storage a-tensor))
              (lwork -1)
              (info 0))
         (when (eql :row-major (layout m))
           (transpose! a-tensor))
         (let ((lda rows)
               (tau (make-array (min rows cols) :element-type ',type))
               (work (make-array (max 1 lwork) :element-type ',type)))
           ;; run it once as a workspace query
           (,rq-function rows cols a lda tau work lwork info)
           (setf lwork (round (realpart (aref work 0))))
           (setf work (make-array (max 1 lwork) :element-type ',type))
           ;; run it again with optimal workspace size
           (,rq-function rows cols a lda tau work lwork info)
           (values a-tensor
                   (from-array tau (list (min rows cols))
                               :type ',type
                               :layout :column-major)))))

     (defmethod lapack-lq ((m ,class))
       (let* ((rows (nrows m))
              (cols (ncols m))
              (a-tensor (deep-copy-tensor m))
              (a (storage a-tensor))
              (lwork -1)
              (info 0))
         (when (eql :row-major (layout m))
           (transpose! a-tensor))
         (let ((lda rows)
               (tau (make-array (min rows cols) :element-type ',type))
               (work (make-array (max 1 lwork) :element-type ',type)))
           ;; run it once as a workspace query
           (,lq-function rows cols a lda tau work lwork info)
           (setf lwork (round (realpart (aref work 0))))
           (setf work (make-array (max 1 lwork) :element-type ',type))
           ;; run it again with optimal workspace size
           (,lq-function rows cols a lda tau work lwork info)
           (values a-tensor
                   (from-array tau (list (min rows cols))
                               :type ',type
                               :layout :column-major)))))

     (defmethod lapack-qr-q ((m ,class) tau)
       (let ((m (nrows m))
             (n (ncols m))
             (a (storage m))
             (k (size tau))
             (atau (storage tau))
             (lwork -1)
             (info 0))
         ;; n replaced with rank (k) to fulfil req (m >= n > 0)
         (let ((lda m)
               (work (make-array (max 1 lwork) :element-type ',type)))
           ;; run it once as a workspace query
           (,qr-q-function m n k a lda atau work lwork info)
           (setf lwork (round (realpart (aref work 0))))
           (setf work (make-array (max 1 lwork) :element-type ',type))
           ;; run it again with optimal workspace size
           (,qr-q-function m n k a lda atau work lwork info)
           (from-array a (list m k) :layout :column-major))))

     (defmethod lapack-ql-q ((m ,class) tau)
       (let ((m (nrows m))
             (n (ncols m))
             (a (storage m))
             (k (size tau))
             (atau (storage tau))
             (lwork -1)
             (info 0))
         (let ((lda m)
               (work (make-array (max 1 lwork) :element-type ',type)))
           ;; run it once as a workspace query
           (,ql-q-function m n k a lda atau work lwork info)
           (setf lwork (round (realpart (aref work 0))))
           (setf work (make-array (max 1 lwork) :element-type ',type))
           ;; run it again with optimal workspace size
           (,ql-q-function m n k a lda atau work lwork info)
           (from-array a (list m n) :layout :column-major))))

     (defmethod lapack-rq-q ((m ,class) tau)
       (let ((m (nrows m))
             (n (ncols m))
             (a (storage m))
             (k (size tau))
             (atau (storage tau))
             (lwork -1)
             (info 0))
         (let ((lda m)
               (work (make-array (max 1 lwork) :element-type ',type)))
           ;; run it once as a workspace query
           (,rq-q-function m n k a lda atau work lwork info)
           (setf lwork (round (realpart (aref work 0))))
           (setf work (make-array (max 1 lwork) :element-type ',type))
           ;; run it again with optimal workspace size
           (,rq-q-function m n k a lda atau work lwork info)
           (from-array a (list m n) :layout :column-major))))

     (defmethod lapack-lq-q ((m ,class) tau)
       (let ((m (nrows m))
             (n (ncols m))
             (a (storage m))
             (k (size tau))
             (atau (storage tau))
             (lwork -1)
             (info 0))
         (let ((lda m)
               (work (make-array (max 1 lwork) :element-type ',type)))
           ;; run it once as a workspace query
           (,lq-q-function m n k a lda atau work lwork info)
           (setf lwork (round (realpart (aref work 0))))
           (setf work (make-array (max 1 lwork) :element-type ',type))
           ;; run it again with optimal workspace size
           (,lq-q-function m n k a lda atau work lwork info)
           (from-array a (list m n) :layout :column-major))))))
