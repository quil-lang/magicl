;;;; matrix.lisp
;;;;
;;;; Author: Cole Scott
;;;;         Robert Smith

(in-package #:magicl)

(deftype matrix-storage (&optional type)
  `(simple-array ,type (*)))

(defclass matrix (abstract-tensor)
  (;; abstract-tensor slots
   (nrows
    :initarg :nrows
    :initform 0
    :reader nrows
    :type alexandria:positive-fixnum
    :documentation "The number of rows in the matrix")
   (ncols
    :initarg :ncols
    :initform 0
    :reader ncols
    :type alexandria:positive-fixnum
    :documentation "The number of columns in the matrix")
   (size
    :initarg :size
    :initform 0
    :reader size
    :type (alexandria:positive-fixnum)
    :documentation "Total number of elements in the matrix")
   (element-type
    :initarg :element-type
    :initform (error "element-type must be specified when creating a matrix instance") ; TODO: much better error messages
    :reader element-type
    :type type
    :documentation "The type of the elements in the matrix")
   ;; matrix-specific slots
   (storage
    :initarg :storage
    :initform (error "storage must be specified when creating a matrix instance")
    :reader storage
    :documentation "Storage of the matrix, typically in a vector in column major order")
   (order
    :initarg :order
    :initform :column-major
    :reader order
    :type (member :row-major :column-major)
    :documentation "Indexing order of storage (:column-major or :row-major)."))
  (:metaclass abstract-class:abstract-class))

(defun pprint-matrix (stream matrix)
  "Pretty-print a matrix MATRIX to the stream STREAM."
  (flet ((print-real (x)
           (format stream "~6,3f" x))
         (print-complex (z)
           (format stream "~6,3f ~:[+~;-~]~6,3fj"
                   (realpart z)
                   (minusp (imagpart z))
                   (abs (imagpart z)))))
    (let* ((rows (nrows matrix))
           (cols (ncols matrix))
           (type (element-type matrix))
           (print-entry #'print-real)) ;; TODO: Check for complex type
      (pprint-logical-block (stream nil)
        (print-unreadable-object (matrix stream :type t)
          (format stream "~Dx~D:" rows cols)
          (dotimes (r rows)
            (pprint-newline :mandatory stream)
            (dotimes (c cols)
              (funcall print-entry (tref matrix r c))
              (unless (cl:= c (1- cols))
                (write-string "    " stream)))))))))

(set-pprint-dispatch 'matrix 'pprint-matrix)

;;; Required abstract-tensor methods

(defmethod rank ((matrix matrix))
  (declare (ignore matrix))
  2)

(defmethod shape ((matrix matrix))
  (list (nrows matrix) (ncols matrix)))

(defmethod tref ((matrix matrix) &rest pos)
  ;; TODO: Check pos type
  (assert (cl:= (rank matrix) (list-length pos))
          () "Invalid index ~a. Must be rank ~a" pos (rank matrix))
  (assert (cl:every #'< pos (shape matrix))
          () "Index ~a out of range" pos)
  (let ((index (ecase (order matrix)
                 (:row-major (cl:+ (second pos) (* (first pos) (ncols matrix))))
                 (:column-major (cl:+ (first pos) (* (second pos) (nrows matrix)))))))
    (aref (storage matrix) index)))

(defmethod (setf tref) (new-value (matrix matrix) &rest pos)
       (assert (cl:= (rank matrix) (list-length pos))
               () "Invalid index ~a. Must be rank ~a" pos (rank matrix))
       (assert (cl:every #'< pos (shape matrix))
               () "Index ~a out of range" pos)
       (let ((index (ecase (order matrix)
                 (:row-major (cl:+ (second pos) (* (first pos) (ncols matrix))))
                 (:column-major (cl:+ (first pos) (* (second pos) (nrows matrix)))))))
         (setf (aref (storage matrix) index) new-value)))

(defmethod copy-tensor ((matrix matrix) &rest args)
  (apply #'make-instance (class-of matrix)
         :nrows (nrows matrix)
         :ncols (ncols matrix)
         :size (size matrix)
         :element-type (element-type matrix)
         :storage (make-array (size matrix)
                              :element-type (element-type matrix))
         args))

(defmethod deep-copy-tensor ((matrix matrix) &rest args)
  (apply #'make-instance (class-of matrix)
         :nrows (nrows matrix)
         :ncols (ncols matrix)
         :size (size matrix)
         :element-type (element-type matrix)
         :storage (alexandria:copy-array (storage matrix))
         args))

;;; Optimized abstract-tensor methods

;; Broken for column-major
#+ignore
(defmethod map! ((function function) (matrix matrix))
  (setf (slot-value matrix 'storage) (cl:map 'vector function (storage matrix)))
  matrix)

;; Also broken
#+ignore
(defmethod into! ((function function) (matrix matrix))
  (let ((i 0))
    (map-indexes
     (shape matrix)
     (lambda (&rest dims)
       (setf (aref (storage matrix) i) (apply function dims))
       (incf i)))
    matrix))

;;; Specfic matrix classes

(defmacro defmatrix (name type tensor-name)
  `(progn
     (defclass ,name (matrix)
       ((storage :type (matrix-storage ,type)))
       (:documentation ,(format nil "Matrix with element type of ~a" type)))
     (defmethod update-instance-for-different-class :before
         ((old ,tensor-name)
          (new ,name)
          &key)
       (assert (cl:= 2 (rank old)))
       (with-slots (shape) old
         (with-slots (nrows ncols) new
           (setf nrows (first shape)
                 ncols (second shape)))))
     (defmethod update-instance-for-different-class :before
         ((old ,name)
          (new ,tensor-name)
          &key)
       (with-slots (nrows ncols) old
         (with-slots (shape rank) new
           (setf shape (list nrows ncols)
                 rank 2))))))

;; Methods to be specified by the specific matrix classes (maybeb)
(defgeneric @ (a b &key target alpha beta transa transb)
    (:documentation "Multiply matrix a by matrix b, storing in target or creating a new matrix if target is not specified.
Target cannot be the same as a or b.")
  (:method (a b &key target alpha beta transa transb)
    (error "Multiplication not defined for this type of matrix (should it be defined genericly? yes. is it? no.).")))

;;; Generic matrix methods

;; Transpose(!)
(defgeneric transpose! (matrix &key fast)
  (:documentation "Transpose a matrix!")
  (:method ((matrix matrix) &key fast)
    "Transpose a matrix by copying values.
If fast is t then just change order. Fast can cause problems when you want to multiply specifying transpose."
    (if fast
        (progn
          (let ((shape (shape matrix)))
            (setf (slot-value matrix 'ncols) (first shape))
            (setf (slot-value matrix 'nrows) (second shape))
            (setf (slot-value matrix 'order) (case (order matrix)
                                               (:row-major :column-major)
                                               (:column-major :row-major)))))
        (let ((index-function
                (ecase (order matrix)
                  (:row-major #'row-major-index)
                  (:column-major #'column-major-index))))
          (loop :for row :below (nrows matrix)
                :do (loop :for col :from row :below (ncols matrix)
                          :do (rotatef
                               (aref (storage matrix) (funcall index-function (list row col) (shape matrix)))
                               (aref (storage matrix) (funcall index-function (list col row) (shape matrix))))))
          (setf (slot-value matrix 'ncols) (first (shape matrix)))
          (setf (slot-value matrix 'nrows) (second (shape matrix)))))
    matrix))

(defgeneric diag (matrix)
  (:documentation "Get a list of the diagonal elements of a matrix")
  (:method ((matrix matrix))
    (assert (= (nrows matrix) (ncols matrix))
            () "Matrix must be square")
    (let ((rows (nrows matrix)))
      (loop :for i :below rows
            :collect (tref matrix i i)))))

(defgeneric inverse (matrix)
  (:documentation "Get the inverse of the matrix"))

(defgeneric trace (matrix)
  (:documentation "Get the trace of the matrix (product of diagonals)")
  (:method ((matrix matrix))
    (assert (= (nrows matrix) (ncols matrix))
            () "Matrix must be square")
    (let ((rows (nrows matrix)))
      (loop :for i :below rows
            :sum (tref matrix i i)))))

;; TODO: maybe write generic form of this
(defgeneric lu (matrix)
  (:documentation "Get the LU decomposition of the matrix"))

(defgeneric det (matrix)
  (:documentation "Compute the determinant of a square matrix")
  (:method ((matrix matrix))
    (assert (= (nrows matrix) (ncols matrix))
            () "Matrix must be square")
    (let ((d 1))
      (multiple-value-bind (a ipiv) (lu matrix)
        (dotimes (i (nrows matrix))
          (setq d (* d (tref a i i))))
        (dotimes (i (size ipiv))
          (unless (= (1+ i) (tref ipiv i))
            (setq d (cl:- d))))
        d))))

(defgeneric conjugate-transpose (matrix)
  (:documentation "Compute the conjugate transpose of a matrix")
  (:method ((matrix matrix))
    (map #'conjugate matrix)))

(defgeneric conjugate-transpose! (matrix)
  (:documentation "Compute the conjugate transpose of a matrix, replacing the elements")
  (:method ((matrix matrix))
    (map! #'conjugate matrix)))

(defgeneric dagger (matrix)
  (:documentation "Compute the conjugate transpose of a matrix")
  (:method ((matrix matrix))
    (conjugate-transpose matrix)))

(defgeneric dagger! (matrix)
  (:documentation "Compute the conjugate transpose of a matrix, replacing the elements")
  (:method ((matrix matrix))
    (conjugate-transpose! matrix)))

;; TODO: either make this generic or call lapack functions
(defgeneric orthonormalize! (matrix)
  (:documentation "Orthonormalize a matrix, replacing the elements"))

;; TODO:
;; Random unitary
;; random (different distributions)
;; diag
;; Inverse
;; einsum
;; Solve
;; Eigenvalues/vectors
;; kron
;; exponent
;; conj-transpose
;; csd
;; svd
;; ql
;; qr
;; rq
;; lq
;; dot
;; get diag elems?
;; stack together

