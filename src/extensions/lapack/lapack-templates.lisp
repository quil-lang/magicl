;;;; lapack-templates.lisp
;;;;
;;;; Author: Cole Scott

;;; NOTE: This file is emulating C++-style templates to generate
;;;       methods to call LAPACK functions. This is done to avoid
;;;       writing the same method four times but results in
;;;       less-than-perfect use of macros.

(in-package #:magicl-lapack)

(defun generate-lapack-mult-for-type (matrix-class vector-class type matrix-matrix-function matrix-vector-function)
  `(progn
     (defmethod mult-extension ((a ,matrix-class) (b ,matrix-class) &key target (alpha ,(coerce 1 type)) (beta ,(coerce 0 type)) (transa :n) (transb :n))
       (policy-cond:with-expectations (> speed safety)
           ((type (member nil :n :t :c) transa)
            (type (member nil :n :t :c) transb))
         (let* ((m (if (eq ':n transa) (nrows a) (ncols a)))
                (k (if (eq ':n transa) (ncols a) (nrows a)))
                (n (if (eq ':n transb) (ncols b) (nrows b)))
                (brows (if (eq ':n transb) (nrows b) (ncols b))))
           (policy-cond:with-expectations (> speed safety)
               ((assertion (cl:= k brows))
                (assertion (or (not target) (equal (shape target) (list m n)))))
             (let ((ta
                     (if (eq ':row-major (layout a))
                         (case transa
                           (:n ':t)
                           (:t ':n)
                           (:c (error "Specifying TRANSA to be :C is not allowed if A is ROW-MAJOR")))
                         transa))
                   (tb (if (eq ':row-major (layout b))
                           (case transb
                             (:n ':t)
                             (:t ':n)
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
                (magicl::storage a)
                (if (eq ':n ta) m k)
                (magicl::storage b)
                (if (eq ':n tb) k n)
                beta
                (magicl::storage target)
                m)
               target)))))
     (defmethod mult-extension ((a ,matrix-class) (x ,vector-class) &key target (alpha ,(coerce 1 type)) (beta ,(coerce 0 type)) (transa :n) transb)
       (policy-cond:with-expectations (> speed safety)
           ((type (member nil :n :t :c) transa)
            (assertion (null transb)))
         (let* ((m-op (if (eq ':n transa) (nrows a) (ncols a)))
                (n-op (if (eq ':n transa) (ncols a) (nrows a))))
           (policy-cond:with-expectations (> speed safety)
               ((assertion (cl:= n-op (size x)))
                (assertion (or (not target) (equal (shape target) (list m-op)))))
             (let ((ta
                     (if (eq ':row-major (layout a))
                         (case transa
                           (:n ':t)
                           (:t ':n)
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
                (if (eq ':column-major (layout a)) (nrows a) (ncols a))
                (if (eq ':column-major (layout a)) (ncols a) (nrows a))
                alpha
                (magicl::storage a)
                (if (eql :column-major (layout a)) (nrows a) (ncols a))
                (magicl::storage x)
                1 ;; NOTE: This corresponds to the stride of X
                beta
                (magicl::storage target)
                1 ;; NOTE: THis corresponds to the stride of TARGET
                )
               target)))))))

(defun generate-lapack-lu-for-type (class type lu-function)
  (declare (ignore type))
  `(defmethod lapack-lu ((a ,class))
     (let* ((a-tensor (deep-copy-tensor a))
            (a (magicl::storage a-tensor))
            (m (nrows a-tensor))
            (n (ncols a-tensor))
            (lda m)
            (ipiv-tensor (empty (list (max m n)) :type '(signed-byte 32)))
            (ipiv (magicl::storage ipiv-tensor))
            (info 0))
       (when (eql :row-major (layout a-tensor)) (transpose! a-tensor))
       (,lu-function
        m
        n
        a
        lda
        ipiv
        info)
       (values a-tensor ipiv-tensor))))

(defun generate-lapack-lu-solve-for-type (class type lu-solve-function)
  (declare (ignore type))
  `(defmethod lapack-lu-solve ((lu ,class) ipiv (b ,class))
     (unless (square-matrix-p lu)
       (error "Expected a square matrix encoding a valid LU factorization."))
     (let* ((a (magicl::storage lu))
            (b-tensor (deep-copy-tensor b))
            (b (magicl::storage b-tensor))
            (trans (ecase (layout lu)
                     (:row-major "T")
                     (:column-major "N")))
            (lda (nrows lu))
            (n lda)
            (ldb (nrows b-tensor))
            (nrhs (ncols b-tensor))
            (ipiv (magicl::storage ipiv))
            (info 0))
       (when (eql :row-major (layout b-tensor)) (transpose! b-tensor))
       (,lu-solve-function
        trans
        n
        nrhs
        a        
        lda
        ipiv
        b
        ldb
        info)
       b-tensor)))

(defun generate-lapack-inv-for-type (class type lu-function inv-function)
  `(defmethod lapack-inv ((a ,class))
     (let ((a-tensor (deep-copy-tensor a)))
       (when (eql :row-major (layout a-tensor)) (transpose! a-tensor))
       (let* ((a (magicl::storage a-tensor))
              (m (nrows a-tensor))
              (n (ncols a-tensor))
              (lda m)
              (ipiv-tensor (empty (list (max m n)) :type '(signed-byte 32)))
              (ipiv (magicl::storage ipiv-tensor))
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
         (values (from-array a (shape a-tensor) :input-layout :column-major))))))

(defun generate-lapack-svd-for-type (class type svd-function &optional real-type)
  `(defmethod lapack-svd ((m ,class) &key reduced)
     "Find the SVD of a matrix M. Return (VALUES U SIGMA Vt) where M = U*SIGMA*Vt. If REDUCED is non-NIL, return the reduced SVD (where either U or V are just partial isometries and not necessarily unitary matrices)."
     (let* ((jobu (if reduced "S" "A"))
            (jobvt (if reduced "S" "A"))
            (rows (nrows m))
            (cols (ncols m))
            (a (magicl::storage (deep-copy-tensor (if (eql :row-major (layout m)) (transpose m) m))))
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
             (setf (aref smat (magicl::matrix-column-major-index i i u-cols vt-rows))
                   (aref s i)))
           (values (from-array u (list rows u-cols) :input-layout :column-major)
                   (from-array smat (list u-cols vt-rows) :input-layout :column-major)
                   (from-array vt (list vt-rows cols) :input-layout :column-major)))))))

;; TODO: This returns only the real parts when with non-complex
;; numbers. Should do something different?
(defun generate-lapack-eig-for-type (class type eig-function &optional real-type)
  ` (defmethod lapack-eig ((m ,class))
      (policy-cond:with-expectations (> speed safety)
          ((assertion (square-matrix-p m)))
        (let ((rows (nrows m))
              (cols (ncols m))
              (a-tensor (deep-copy-tensor m)))
          (when (eql :row-major (layout m)) (transpose! a-tensor))
          (let ((jobvl "N")
                (jobvr "V")
                (a (magicl::storage a-tensor))
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
              (values (coerce ,@(if real-type `(w) `(wr)) 'list) (from-array vr (list rows cols) :input-layout :column-major))))))))

(defun generate-lapack-hermitian-eig-for-type (class type eig-function real-type)
  `(defmethod lapack-hermitian-eig ((m ,class))
     (policy-cond:with-expectations (> speed safety)
         ((assertion (square-matrix-p m))
          (assertion (hermitian-matrix-p m)))
       (let ((rows (nrows m))
             (a-tensor (deep-copy-tensor m)))
         (when (eql :row-major (layout m)) (transpose! a-tensor))
         (let ((jobz "V")
               (uplo "U")
               (n rows)
               (a (magicl::storage a-tensor))
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
           (values (coerce w 'list) a-tensor))))))

;; TODO: implement row-major checks in these functions
(defun generate-lapack-ql-qr-rq-lq-for-type (class type
                                             ql-function qr-function rq-function lq-function
                                             ql-q-function qr-q-function rq-q-function lq-q-function)
  `(progn
     (defmethod qr-extension ((m ,class))
       (policy-cond:with-expectations (> speed safety)
           ;; Needed for LAPACK-QR to do its job. In principle DGEQRF
           ;; doesn't impose this, but the results are represented
           ;; slightly differently for the cols <= rows vs rows < cols
           ;; cases, and we only handle the former.
           ((assertion (<= (ncols m) (nrows m))))
         (let ((rows (nrows m))
               (cols (ncols m)))
           (multiple-value-bind (a tau) (lapack-qr m)
             (let* ((r (upper-triangular a :square t))
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

     (defmethod ql-extension ((m ,class))
       (policy-cond:with-expectations (> speed safety)
           ;; Similar to the assert for QR above.
           ((assertion (<= (ncols m) (nrows m))))
         (let ((rows (nrows m))
               (cols (ncols m)))
           (multiple-value-bind (a tau) (lapack-ql m)
             (let* ((l (lower-triangular a :square t))
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

     (defmethod rq-extension ((m ,class))
       (policy-cond:with-expectations (> speed safety)
           ;; Similar to the assert for QR above.
           ((assertion (>= (ncols m) (nrows m))))
         (let ((rows (nrows m))
               (cols (ncols m)))
           (multiple-value-bind (a tau) (lapack-rq m)
             (let* ((r (upper-triangular a :square t))
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
               (values r q))))))

     (defmethod lq-extension ((m ,class))
       (policy-cond:with-expectations (> speed safety)
           ;; Similar to the assert for QR above.
           ((assertion (>= (ncols m) (nrows m))))
         (let ((rows (nrows m))
               (cols (ncols m)))
           (multiple-value-bind (a tau) (lapack-lq m)
             (let* ((l (lower-triangular a :square t))
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
               (values l q))))))

     (defmethod lapack-qr ((m ,class))
       (let* ((rows (nrows m))
              (cols (ncols m))
              (a-tensor (deep-copy-tensor m))
              (a (magicl::storage a-tensor))
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
                               :input-layout :column-major)))))

     (defmethod lapack-ql ((m ,class))
       (let* ((rows (nrows m))
              (cols (ncols m))
              (a-tensor (deep-copy-tensor m))
              (a (magicl::storage a-tensor))
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
                               :input-layout :column-major)))))

     (defmethod lapack-rq ((m ,class))
       (let* ((rows (nrows m))
              (cols (ncols m))
              (a-tensor (deep-copy-tensor m))
              (a (magicl::storage a-tensor))
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
                               :input-layout :column-major)))))

     (defmethod lapack-lq ((m ,class))
       (let* ((rows (nrows m))
              (cols (ncols m))
              (a-tensor (deep-copy-tensor m))
              (a (magicl::storage a-tensor))
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
                               :input-layout :column-major)))))

     (defmethod lapack-qr-q ((m ,class) tau)
       (let ((m (nrows m))
             (n (ncols m))
             (a (magicl::storage m))
             (k (size tau))
             (atau (magicl::storage tau))
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
           (from-array a (list m k) :input-layout :column-major))))

     (defmethod lapack-ql-q ((m ,class) tau)
       (let ((m (nrows m))
             (n (ncols m))
             (a (magicl::storage m))
             (k (size tau))
             (atau (magicl::storage tau))
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
           (from-array a (list m n) :input-layout :column-major))))

     (defmethod lapack-rq-q ((m ,class) tau)
       (let ((m (nrows m))
             (n (ncols m))
             (a (magicl::storage m))
             (k (size tau))
             (atau (magicl::storage tau))
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
           (from-array a (list m n) :input-layout :column-major))))

     (defmethod lapack-lq-q ((m ,class) tau)
       (let ((m (nrows m))
             (n (ncols m))
             (a (magicl::storage m))
             (k (size tau))
             (atau (magicl::storage tau))
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
           (from-array a (list m n) :input-layout :column-major))))))
