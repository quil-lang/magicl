;;;; lapack-macros.lisp
;;;;
;;;; Author: Cole Scott

(in-package #:magicl)

(defmacro def-lapack-mult (class type blas-function)
  `(defmethod mult ((a ,class) (b ,class) &key target (alpha ,(coerce 1 type)) (beta ,(coerce 0 type)) (transa :n) (transb :n))
     (check-type transa (member nil :n :t :c))
     (check-type transb (member nil :n :t :c))
     (let* ((m (if (eq :n transa) (nrows a) (ncols a)))
            (k (if (eq :n transa) (ncols a) (nrows a)))
            (n (if (eq :n transb) (ncols b) (nrows b)))
            (brows (if (eq :n transb) (nrows b) (ncols b))))
       (assert (cl:= k brows)
               () "Incompatible matrix sizes ~a and ~a." (list m k) (list brows n))
       (when target
         (assert (equal (shape target) (list m n))
                 () "Incompatible target shape. Target needs shape ~a but has shape ~a"
                 (shape target) (list m n)))
       (let ((ta
               (if (eql :row-major (order a))
                   (case transa
                     (:n :t)
                     (:t :n)
                     (:c (error "Specifying TRANSA to be :C is not allowed if A is ROW-MAJOR")))
                   transa))
             (tb (if (eql :row-major (order b))
                     (case transb
                       (:n :t)
                       (:t :n)
                       (:c (error "Specifying TRANSB to be :C is not allowed if B is ROW-MAJOR")))
                     transb))
             (target (or target
                         (empty
                          (list m n)
                          :type ',type))))
         (,blas-function
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
         target))))

(defmacro def-lapack-lu (class type lu-function)
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
         (when (eql :row-major (order a-tensor)) (transpose! a-tensor))
         (,lu-function
          m
          n
          a
          lda
          ipiv
          info)
         (values a-tensor ipiv-tensor)))))

;; NOTE: This requires lu to be defined
(defmacro def-lapack-inv (class type lu-function inv-function)
  `(progn
     (defmethod inverse ((m ,class))
       (lapack-inv m))
     
     (defmethod lapack-inv ((a ,class))
       (let ((a-tensor (deep-copy-tensor a)))
         (when (eql :row-major (order a-tensor)) (transpose! a-tensor))
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

(defmacro def-lapack-svd (class type svd-function &optional real-type)
  `(progn
     (defmethod svd ((m ,class))
       (lapack-svd m))
     
     (defmethod lapack-svd ((m ,class))
       "Find the SVD of a matrix M. Return (VALUES U SIGMA Vt) where M = U*SIGMA*Vt"
       (let ((jobu "A")
             (jobvt "A")
             (rows (nrows m))
             (cols (ncols m))
             (a (alexandria:copy-array (storage (if (eql :row-major (order m)) (transpose m) m))))
             (lwork -1)
             (info 0))
         (let ((lda rows)
               (s (make-array (min rows cols) :element-type ',(or real-type type)))
               (ldu rows)
               (ldvt cols)
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
             (let ((smat (make-array (* rows cols) :element-type ',(or real-type type))))
               (dotimes (i (min rows cols))
                 (setf (aref smat (column-major-index (list i i) (shape m)))
                       (aref s i)))
               (values (from-array u (list rows rows) :order :column-major)
                       (from-array smat (list rows cols) :order :column-major)
                       (from-array vt (list cols cols) :order :column-major)))))))))

;; TODO: This returns only the real parts when with non-complex numbers. Should do something different?
(defmacro def-lapack-eig (class type eig-function &optional real-type)
  `(progn
     (defmethod eig ((m ,class))
       (lapack-eig m))

     (defmethod lapack-eig ((m ,class))
       (assert-square-matrix m)
       (let ((rows (nrows m))
             (cols (ncols m))
             (a-tensor (deep-copy-tensor m)))
         (when (eql :row-major (order m)) (transpose! a-tensor))
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
             (values (coerce ,@(if real-type `(w) `(wr)) 'list) (from-array vr (list rows cols) :order :column-major))))))))

;; TODO: implement row-major checks in these functions
(defmacro def-lapack-ql-qr-rq-lq (class type
                                  ql-function qr-function rq-function lq-function
                                  ql-q-function qr-q-function rq-q-function lq-q-function)
  `(progn
     (defmethod qr ((m ,class))
       (assert-square-matrix m)
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
                       (setf (tref r j i) (cl:- (tref r j i))))
                     (setf (tref q i j) (cl:- (tref q i j)))))))
             (values q r)))))
     
     (defmethod ql ((m ,class))
       (assert-square-matrix m)
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
                       (setf (tref l j i) (cl:- (tref l j i))))
                     (setf (tref q i j) (cl:- (tref q i j)))))))
             (values q l)))))

     (defmethod rq ((m ,class))
       (assert-square-matrix m)
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
                       (setf (tref r j i) (cl:- (tref r j i))))
                     (setf (tref q i j) (cl:- (tref q i j)))))))
             (values q r)))))

     (defmethod lq ((m ,class))
       (assert-square-matrix m)
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
                       (setf (tref l j i) (cl:- (tref l j i))))
                     (setf (tref q i j) (cl:- (tref q i j)))))))
             (values q l)))))

     (defmethod lapack-qr ((m ,class))
       (let* ((rows (nrows m))
              (cols (ncols m))
              (a-tensor (deep-copy-tensor m))
              (a (storage a-tensor))
              (lwork -1)
              (info 0))
         (when (eql :row-major (order m))
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
                               :order :column-major)))))
     
     (defmethod lapack-ql ((m ,class))
       (let* ((rows (nrows m))
              (cols (ncols m))
              (a-tensor (deep-copy-tensor m))
              (a (storage a-tensor))
              (lwork -1)
              (info 0))
         (when (eql :row-major (order m))
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
                               :order :column-major)))))

     (defmethod lapack-rq ((m ,class))
       (let* ((rows (nrows m))
              (cols (ncols m))
              (a-tensor (deep-copy-tensor m))
              (a (storage a-tensor))
              (lwork -1)
              (info 0))
         (when (eql :row-major (order m))
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
                               :order :column-major)))))

     (defmethod lapack-lq ((m ,class))
       (let* ((rows (nrows m))
              (cols (ncols m))
              (a-tensor (deep-copy-tensor m))
              (a (storage a-tensor))
              (lwork -1)
              (info 0))
         (when (eql :row-major (order m))
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
                               :order :column-major)))))

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
           (from-array a (list m k) :order :column-major))))

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
           (from-array a (list m n) :order :column-major))))

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
           (from-array a (list m n) :order :column-major))))

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
           (from-array a (list m n) :order :column-major))))))
