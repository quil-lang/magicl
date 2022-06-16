;;;; givens.lisp
;;;;
;;;; Author: Erik Davis

(in-package #:magicl)

;;;; This file doesn't actually implement any MAGICL functions. These
;;;; functions are utilities for implementing other functions.

(defun givens-entries (a b)
  "Compute the entries of the Givens matrix which rotates a vector to the x-axis.
Returns C and S such that [C -S; S C] @ [a; b] = [r; 0]."
  (cond ((zerop b)
         (values 1 0))
        ((> (abs b) (abs a))
         (let* ((tau (- (/ a b)))
                (s (/ (sqrt (+ 1 (* tau tau)))))
                (c (* s tau)))
           (values c s)))
        (t
         (let* ((tau (- (/ b a)))
                (c (/ (sqrt (+ 1 (* tau tau)))))
                (s (* c tau)))
           (values c s)))))

(defstruct givens-rotation
  "Representation of a Givens rotation between axis I and J, with C = cos(theta), S = sin(theta)."
  i j c s)


(defun left-apply-givens! (A givens &key (start-idx 0) trans)
  "Apply the Givens rotation to the matrix A, on the left (i.e. as row rotations).
Only updates columns with index >= START-IDX."
  (let ((n (ncols A))
        (i (givens-rotation-i givens))
        (j (givens-rotation-j givens))
        (c (givens-rotation-c givens))
        (s (givens-rotation-s givens)))
    (when trans
      (setf s (- s)))
    (loop :for p :from start-idx :below n
          :for t1 := (tref A i p)
          :for t2 := (tref A j p)
          :do (setf (tref A i p) (- (* c t1) (* s t2))
                    (tref A j p) (+ (* s t1) (* c t2))))
    A))

(defun right-apply-givens! (A givens &key (start-idx 0) trans)
  "Apply the Givens rotation to the matrix A, on the right (i.e. as column rotations).
Only updates rows with index >= START-IDX."
  (let ((m (nrows A))
        (i (givens-rotation-i givens))
        (j (givens-rotation-j givens))
        (c (givens-rotation-c givens))
        (s (givens-rotation-s givens)))
    (when trans
      (setf s (- s)))
    (loop :for p :from start-idx :below m
          :for t1 := (tref A p i)
          :for t2 := (tref A p j)
          :do (setf (tref A p i) (- (* c t1) (* s t2))
                    (tref A p j) (+ (* s t1) (* c t2))))
    A))
