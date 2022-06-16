;;;; householder.lisp
;;;;
;;;; Author: Erik Davis

(in-package #:magicl)

;;;; This file doesn't actually implement any MAGICL functions. These
;;;; functions are utilities for implementing QR decomposition.

(defstruct householder-reflection
  "Representation of a Householder reflection."
  v (idx 0))

(defun column->householder (A i0 j0)
  "Construct a Householder reflection which, when applied from the left, puts zeros in column J0 below row I0."
  (let* ((m (nrows A))      
         (v (zeros (list (- m i0)) :type (element-type A))))
    (loop :for i :from i0 :below m
          :for iv :from 0
          :do (setf (tref v iv) (tref A i j0)))
    (let ((norm-v (norm v)))
      (if (zerop norm-v)
          (make-householder-reflection)
          (let ((v0 (tref v 0)))
            (incf (tref v 0)                ; TODO: revisit this
                  (* norm-v (/ v0 (abs v0))))
            (scale! v (/ (norm v)))
            (make-householder-reflection :v v :idx j0))))))

(defun row->householder (A i0 j0)
  "Construct a Householder reflection which, when applied from the right, puts zeros in row I0 to the right of column J0."
  (let* ((n (ncols A))
         (v (zeros (list (- n j0)) :type (element-type A))))
    (loop :for j :from j0 :below n
          :for jv :from 0
          :do (setf (tref v jv) (conjugate (tref A i0 j))))
    (let ((norm-v (norm v)))
      (if (zerop norm-v)
          (make-householder-reflection)
          (let ((v0 (tref v 0)))
            (incf (tref v 0)
                  (* norm-v (/ v0 (abs v0))))
            (scale! v (/ (norm v)))
            (make-householder-reflection :v v :idx i0))))))

(defun left-apply-householder! (A hh)
  "Apply the Householder reflection HH to A[i:,j:], from the left."
  (when (null (householder-reflection-v hh))
    (return-from left-apply-householder! A))
  (let* ((m (nrows A))
         (n (ncols A))
         (v (householder-reflection-v hh))
         (i0 (- m (size v))))
    (flet ((column-reflect! (j)
             "Reflect A[i0:m,j]."
             (let ((v-dot-A
                     (loop :for i :from i0 :below m
                           :for iv :from 0
                           :sum (* (conjugate (tref v iv)) (tref A i j)))))
               (loop :for i :from i0 :below m
                     :for iv :from 0
                     :do (decf (tref A i j)
                               (* 2 (tref v iv) v-dot-A))))))
      (loop :for j :from (householder-reflection-idx hh) :below n
            :do (column-reflect! j))))
  A)

(defun right-apply-householder! (A hh)
  "Apply the Householder reflection HH to A[i:,j:] from the right."
  (declare (optimize debug))
  (when (null (householder-reflection-v hh))
    (return-from right-apply-householder! A))
  (let* ((m (nrows A))
         (n (ncols A))
         (v (householder-reflection-v hh))
         (j0 (- n (size v))))
    (flet ((row-reflect! (i)
             "Reflect A[i,j0:m] across (dagger V)."
             (let ((v*-dot-a
                     (loop :for j :from j0 :below n
                           :for iv :from 0
                           :sum (* (tref A i j) (tref v iv)))))
               (loop :for j :from j0 :below n
                     :for iv :from 0
                     :do (decf (tref A i j)
                               (* 2 v*-dot-a (conjugate (tref v iv))))))))
      (loop :for i :from (householder-reflection-idx hh) :below m
            :do (row-reflect! i)))))
  
