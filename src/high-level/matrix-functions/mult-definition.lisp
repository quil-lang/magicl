;;; mult-definition.lisp
;;;
;;; Author: Erik Davis

(in-package #:magicl)

;;; The two things of relevance to consumers here are the macros REGISTER-MATRIX-MATRIX-MULTIPLY 
;;; and REGISTER-MATRIX-VECTOR-MULTIPLY, which define type-specialized methods for multiplication.
;;; These methodds both look something like this:
;;;
;;;   (labels ((variant-0 (a b ...) ...)
;;;            (variant-1 (a b ...) ...)
;;;            ...)
;;;     (ecase (dispatch-index layouta trans ...)
;;;       (0 (variant-0 a b ...))
;;;       (1 (variant-1 a b ...))
;;;       ...))
;;;
;;; where LAYOUTA is one of :ROW-MAJOR or :COLUMN-MAJOR, TRANSA is one of :N (no transpose),
;;; :T (transpose), :C (conjugate transpose). This is done so that the individual variants
;;; may explicity declare types and do fast array lookups, bypassing the slower generic
;;; machinery of TREF etc. Note that under the hood, :T and :C cases are handled by
;;; doing a "fast transpose" (i.e. flipping the layout and dimensions of a shallow copy)
;;; so that subsequently we only need to consider layout information for resolving
;;; array offsets. Hence within the helper routines below, :C often refers to a simple
;;; 'conjugate' rather than 'conjugate transpose' operation.

(defun %matrix-ref-op (layout trans)
  "Construct an operator for resolving matrix references, given LAYOUT and TRANS.
Here TRANS controls the interpretation of matrix entries, and may be one of
  :N - normal
  :C - conjugate."
  (check-type layout (member :row-major :column-major))
  (check-type trans (member :n :c))
  (lambda (storage row col numrows numcols)
    (let ((args (list trans layout)))
      (cond
        ((equal args '(:n :row-major))
         `(aref ,storage (+ ,col (the fixnum (* ,row ,numcols)))))
        ((equal args '(:n :column-major))
         `(aref ,storage (+ ,row (the fixnum (* ,col ,numrows)))))
        ((equal args '(:c :row-major))
         `(conjugate
           (aref ,storage (+ ,col (the fixnum (* ,row ,numcols))))))
        ((equal args '(:c :column-major))
         `(conjugate
           (aref ,storage (+ ,row (the fixnum (* ,col ,numrows))))))
        (t (error "Unexpected arguments for MATRIX-REF-OP"))))))

(defun %matmul-dispatch-index (layouta transa layoutb transb layout)
  "Get the index into the matrix-matrix multiplication dispatch table."
  (let ((layouts '(:row-major :column-major))
        (flags '(:n :c)))
    (reduce (lambda (n val-elts)
              (destructuring-bind (val elts) val-elts
                (+ (position val elts)
                   (* n (length elts)))))
            (mapcar #'list
                    (list layouta transa layoutb transb layout)
                    (list layouts flags layouts flags layouts))
            :initial-value 0)))

(defun %matmul-specialization (element-type layouta transa layoutb transb layout)
  "Construct a matrix-matrix multiply, specialized on type, layout, and transpose information.
Returns two values: the dispatch index, and a corresponding LABELS entry."
  (let* ((idx (%matmul-dispatch-index layouta transa layoutb transb layout))
         (name
           (intern (format nil "%MATMUL-~A-~D"
                           element-type
                           idx))))
    (flet ((loop-over-indices (i j k max-i max-j max-k)
	     ;; basic strategy: the inner loop should walk along
	     ;; contiguous data (e.g. along rows for a column major
	     ;; matrix).  furthermore, since we are writing to the
	     ;; target matrix, we don't want the middle loop to vary
	     ;; an index of the target matrix, as this would force
	     ;; these writes out of cache.  note: this heuristic is
	     ;; really best when all three matrices have the same
	     ;; layout...
	     (let ((outer (ecase layout
			    (:row-major (list i max-i))
			    (:column-major (list j max-j))))
		   (middle (list k max-k))
		   (inner (ecase layout
			    (:row-major (list j max-j))
			    (:column-major (list i max-i)))))
	       (lambda (body)
                 `(dotimes ,outer
                    (declare (type fixnum ,(first outer)))
                    (dotimes ,middle
		      (declare (type fixnum ,(first middle)))
		      (dotimes ,inner
                        (declare (type fixnum ,(first inner)))
                        ,body)))))))
      (values
       idx
       `(,name (a b target alpha beta)
               (declare (type ,element-type alpha beta))
               (let* ((m (nrows a))
                      (n (ncols b))
                      (p (ncols a))
                      (q (nrows b))
                      (target (or target (zeros (list m n) :type ',element-type :layout ,layout)))
                      (storage-a (storage a))
                      (storage-b (storage b))
                      (storage-target (storage target)))
                 (declare (type fixnum m n p q)
                          (type (matrix-storage ,element-type) storage-a storage-b storage-target))
                 (policy-cond:with-expectations (> speed safety)
                     ((assertion (cl:= p q))
                      (assertion (equal (shape target)
                                        (list m n))))
                   (scale! target beta)
                   ,(funcall (loop-over-indices 'i 'j 'k 'm 'n 'p)
                             `(incf ,(funcall (%matrix-ref-op layout :n)
                                              'storage-target 'i 'j 'm 'n)
                                    (* alpha
                                       ,(funcall (%matrix-ref-op layouta transa)
                                                 'storage-a 'i 'k 'm 'p)
                                       ,(funcall (%matrix-ref-op layoutb transb)
                                                 'storage-b 'k 'j 'q 'n))))

                   target)))))))

(defun %matmul-dispatch (element-type a b target alpha beta transa transb)
  "Construct a matrix-matrix multiply, which dispatches to specialized routines."
  (let ((specializations nil))
    (flet ((specialize (layouta transa layoutb transb layout)
             (multiple-value-bind (idx label)
                 (%matmul-specialization element-type layouta transa layoutb transb layout)
               (push (cons idx label) specializations)
               label)))
      `(let* ((a ,a)
              (b ,b)
              (target ,target)
              (alpha (coerce ,alpha ',element-type))
              (beta (coerce ,beta ',element-type))
              (layouta (layout a))
              (transa ,transa)
              (layoutb (layout b))
              (transb ,transb)
              (layout (or (and target (layout target))
                          (layout a))))
         (policy-cond:with-expectations (> speed safety)
             ((type (member :n :c) transa)
              (type (member :n :c) transb))     
           (labels
               (,@(alexandria:map-product         
                      #'specialize
                    '(:row-major :column-major) '(:n :c)
                    '(:row-major :column-major) '(:n :c)
                    '(:row-major :column-major)))
             (ecase (%matmul-dispatch-index layouta transa layoutb transb layout)
               ,@(loop :for (idx . label) :in (nreverse specializations)
                       :for name := (first label)
                       :collect `(,idx (,name a b target alpha beta))))))))))


(defun %vecmul-dispatch-index (layouta transa layout)
  "Get the index into the matrix-vector multiplication dispatch table."
  (let ((layouts '(:row-major :column-major))
        (flags '(:n :c)))
    (reduce (lambda (n val-elts)
              (destructuring-bind (val elts) val-elts
                (+ (position val elts)
                   (* n (length elts)))))
            (mapcar #'list
                    (list layouta transa layout)
                    (list layouts flags layouts))
            :initial-value 0)))

(defun %vecmul-specialization (element-type layouta transa layout)
  "Construct a matrix-vector multiply, specialized on type, layout, and transpose information.
Returns two values: the dispatch index, and a corresponding LABELS entry."
  (let* ((idx (%vecmul-dispatch-index layouta transa layout))
         (name
           (intern (format nil "%VECMUL-~A-~D"
                           element-type
                           idx))))
    (flet ((loop-over-indices (i j max-i max-j)
             (lambda (body)
               `(dotimes (,i ,max-i)
                  (declare (type fixnum ,i))
                  (dotimes (,j ,max-j)
                    (declare (type fixnum ,j))
                    ,body)))))
      (values
       idx
       `(,name (a b target alpha beta)
               (declare (type ,element-type alpha beta))
               (let* ((m (nrows a))
                      (p (ncols a))
                      (q (size b))
                      (target (or target (zeros (list m) :type ',element-type)))
                      (storage-a (storage a))
                      (storage-b (storage b))
                      (storage-target (storage target)))
                 (declare (type fixnum m p q)
                          (type (matrix-storage ,element-type) storage-a storage-b storage-target))
                 (policy-cond:with-expectations (> speed safety)
                     ((assertion (cl:= p q))
                      (assertion (equal (shape target)
                                        (list m))))
                   (scale! target beta)
                   ,(funcall (loop-over-indices 'i 'j 'm 'p)
                             `(incf (aref storage-target i)
                                    (* alpha
                                       ,(funcall (%matrix-ref-op layouta transa)
                                                 'storage-a 'i 'j 'm 'p)
                                       (aref storage-b j))))

                   target)))))))

(defun %vecmul-dispatch (element-type a b target alpha beta transa)
  "Construct a matrix-matrix multiply, which dispatches to specialized routines."
  (let ((specializations nil))
    (flet ((specialize (layouta transa layout)
             (multiple-value-bind (idx label)
                 (%vecmul-specialization element-type layouta transa layout)
               (push (cons idx label) specializations)
               label)))
      `(let* ((a ,a)
              (b ,b)
              (target ,target)
              (alpha (coerce ,alpha ',element-type))
              (beta (coerce ,beta ',element-type))
              (layouta (layout a))
              (transa ,transa)
              (layout (or (and target (layout target))
                          (layout a))))
         (policy-cond:with-expectations (> speed safety)
             ((type (member :n :c) transa))
           (labels
               (,@(alexandria:map-product
                      #'specialize
                    '(:row-major :column-major) '(:n :c)
                    '(:row-major :column-major)))
             (ecase (%vecmul-dispatch-index layouta transa layout)
               ,@(loop :for (idx . label) :in (nreverse specializations)
                       :for name := (first label)
                       :collect `(,idx (,name a b target alpha beta))))))))))

(defun %precompute-transpose (matrix-type mat trans)
  "Generate an expression to precompute the 'transpose' part of TRANS, applied to MAT.
This is done so that the only TRANS kinds which must be subsequently handled are
  :N - normal matrix reference
"
  (flet ((shallow-copy (mat)
	   `(,(intern (format nil "COPY-~:@(~A~)" matrix-type)) ,mat)))
    `(unless (eq :n ,trans)
       (setf ,mat ,(shallow-copy mat))
       (transpose! ,mat :fast t)
       (when (eq :t ,trans)
	 (setf ,trans :n)))))


(defmacro register-matrix-matrix-multiply (matrix-type element-type)
  "Add a specialized matrix-multiply, corresponding to the given MATRIX-TYPE and ELEMENT-TYPE."
  `(defmethod mult-lisp ((a ,matrix-type) (b ,matrix-type) &key target (alpha 1) (beta 0) (transa :n) (transb :n))
     ,(%precompute-transpose matrix-type 'a 'transa)
     ,(%precompute-transpose matrix-type 'b 'transb)
     ,(%matmul-dispatch `,element-type 'a 'b 'target 'alpha 'beta 'transa 'transb)))

(defmacro register-matrix-vector-multiply (matrix-type vector-type element-type)
  "Add a specialized matrix-multiply, corresponding to the given MATRIX-TYPE and ELEMENT-TYPE."
  `(defmethod mult-lisp ((a ,matrix-type) (b ,vector-type) &key target (alpha 1) (beta 0) (transa :n) transb)
     (declare (ignore transb))
     ,(%precompute-transpose matrix-type 'a 'transa)
     ,(%vecmul-dispatch `,element-type 'a 'b 'target 'alpha 'beta 'transa)))
