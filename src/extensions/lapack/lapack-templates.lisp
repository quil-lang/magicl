;;;; lapack-templates.lisp
;;;;
;;;; Author: Cole Scott

;;; NOTE: This file is emulating C++-style templates to generate
;;;       methods to call LAPACK functions. This is done to avoid
;;;       writing the same method four times but results in
;;;       less-than-perfect use of macros.

(in-package #:magicl-lapack)

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
