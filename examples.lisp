;; Copyright rif 2006.
;; Modified BSD License (see LICENSE file in this directory).

(defpackage :org.middleangle.cl-blapack-examples
  (:nicknames :blapack-examples)
  (:use :common-lisp :fnv :fnv-utils :cl-blapack)
  (:export :simple-example))

(in-package :org.middleangle.cl-blapack-examples)

;; This is a demonstration of the features of the CL blapack
;; interface.  We are working with BLAS/LAPACK (henceforth blapack) at
;; the FORTRAN level, so there are no such things as matrices.  All we
;; have is vectors.  This should show you how the interface works, and
;; should make clear why we'd like another layer on top.


(defun simple-example ()
  ;; Set the traps
  (sb-int:with-float-traps-masked (:divide-by-zero)

    ;; A simple dot product example
    (let ((a (make-fnv-double 4 :initial-value 1.0d0))
	  (b (make-fnv-double 4 :initial-value 2.0d0)))
      (format t "a^t = ~A~%b^t = ~A~%a^t b = ~A~%~%" 
	      a b (%ddot 10 a 1 b 1)))
    
    ;; An eigenvalue example.  Note that we have no matrix abstraction a
    ;; this point.  We pretend 4-vectors are 2-by-2 matrices.
    
    ;; BLAS/LAPACK expects column major order, we are creating the
    ;; (matlab notation) matrix M = [1 2; 2 3].
    (let ((M (make-fnv-double 4)))
      (setf (fnv-double-ref M 0) 1.0d0
	    (fnv-double-ref M 1) 2.0d0
	    (fnv-double-ref M 2) 2.0d0
	    (fnv-double-ref M 3) 3.0d0)
    
      (let ((V (make-fnv-double 4))
	    (D (make-fnv-double 2))
	    (lwork 4096)
	    (liwork 4096)
	    (info (make-fnv-int32 1))
	    (eigs-found (make-fnv-int32 1)))	    
	
	(%dsyevr "V" "A" "U" 2 (copy-fnv-double M) 2 0.0d0 0.0d0 
		 0 0 -1.0d0  eigs-found D V 2 (make-fnv-int32 4) 
		 (make-fnv-double lwork) lwork 
		 (make-fnv-int32 liwork) liwork
		 info)
	(format t "M = ~A~%V=~A~%D=~A~%~%" M V D)
	
	;; Construct a "matlab-style D" --- is there a better way?
	(let ((Df (make-fnv-double 4 :initial-value 0.0d0)))
	  (setf (fnv-double-ref Df 0) (fnv-double-ref D 0)
		(fnv-double-ref Df 3) (fnv-double-ref D 1))
	  ;; Reconstruct M as V*Df*V';
	  (let ((Mri (make-fnv-double 4))
		(Mr (make-fnv-double 4)))
	    (%dgemm "N" "N" 2 2 2 1.0d0 V 2 Df 2 0.0d0 Mri 2)
	    (%dgemm "N" "T" 2 2 2 1.0d0 Mri 2 V 2 0.0d0 Mr 2)
	    (format t "Reconstructed M = ~A~%" Mr)))))))

;; (simple-example)