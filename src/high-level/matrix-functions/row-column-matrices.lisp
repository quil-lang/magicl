;;;; row-column-matrices.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:magicl)

(macrolet ((define-vector-matrix-conversion (type)
             (let* ((vec-type (alexandria:symbolicate "VECTOR/" type))
                    (mat-type (alexandria:symbolicate "MATRIX/" type))
                    (make-vec (alexandria:symbolicate "MAKE-" vec-type))
                    (make-mat (alexandria:symbolicate "MAKE-" mat-type)))
               `(progn
                  (defmethod vector->row-matrix ((vector ,vec-type))
                    (,make-mat 1 (size vector) (size vector) ':column-major (storage vector)))
                  (defmethod vector->column-matrix ((vector ,vec-type))
                    (,make-mat (size vector) 1 (size vector) ':column-major (storage vector)))
                  (defmethod row-matrix->vector ((matrix ,mat-type))
                    (assert (= 1 (nrows matrix)))
                    (,make-vec (ncols matrix) (storage matrix)))
                  (defmethod column-matrix->vector ((matrix ,mat-type))
                    (assert (= 1 (ncols matrix)))
                    (,make-vec (nrows matrix) (storage matrix)))))))
  (define-vector-matrix-conversion single-float)
  (define-vector-matrix-conversion double-float)
  (define-vector-matrix-conversion complex-single-float)
  (define-vector-matrix-conversion complex-double-float)
  (define-vector-matrix-conversion int32))
