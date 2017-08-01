(defpackage #:magicl-examples
  (:use :common-lisp :fnv :fnv-utils :magicl)
  (:export :dot-example :eigenvalue-example))

(in-package #:magicl-examples)

;; This is a demonstration of the features of the CL blapack
;; interface.  We are working with BLAS/LAPACK (henceforth blapack) at
;; the FORTRAN level, so there are no such things as matrices.  All we
;; have is vectors.  This should show you how the interface works, and
;; should make clear why we'd like another layer on top.

(defun dot (u v)
  (assert (= (length u) (length v)) (u v))
  (let ((n (length u)))
    (sb-int:with-float-traps-masked (:divide-by-zero :underflow :overflow :inexact :invalid)
      (let ((cx (fnv:make-fnv-complex-double n))
            (cy (fnv:make-fnv-complex-double n)))
        (dotimes (i n)
          (setf (fnv:fnv-complex-double-ref cx i) (aref u i)
                (fnv:fnv-complex-double-ref cy i) (aref v i)))
        (format t "x: ~A~%y: ~A~%" cx cy)
        (magicl.blas-cffi::%zdotc
         n
         cx
         1
         cy
         1)))))

(defun dot-example ()
  (let ((a (fnv:make-fnv-complex-float 4 :initial-value (complex 1.0e0)))
        (b (fnv:make-fnv-complex-float 4 :initial-value (complex 2.0e0))))
    (format t "a^t = ~A~%b^t = ~A~%a^t b = ~A~%~%"
            a b (magicl.blas-cffi::%cdotu 4 a 1 b 1))))

(defun eigenvalue-example ()
  ;; Set the traps
  (sb-int:with-float-traps-masked (:divide-by-zero :invalid)
    
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
	    (info 0)
	    (eigs-found 0))

	(magicl.lapack-cffi::%dsyevr "V" "A" "U" 2 (copy-fnv-double M) 2 0.0d0 0.0d0
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

(defun qr-example ()
  (multiple-value-bind (q r)
      (qr (make-complex-matrix 4 3 -1 1 -1 1 -1 3 -1 3 1 3 5 7))
    (print "Q")
    (princ '#\Newline)
    (print-matrix q)
    (print "R")
    (princ '#\Newline)
    (print-matrix r)))

(defun svd-example ()
  (multiple-value-bind (u sigma vt) 
            (svd (make-complex-matrix 2 3 3 2 2 3 2 -2))
          (print "U")
          (princ '#\Newline)
          (print-matrix u)
          (print "SIGMA")
          (princ '#\Newline)
          (print-matrix sigma)
          (princ '#\Newline)
          (princ "VT")
          (print-matrix vt)
        ))
