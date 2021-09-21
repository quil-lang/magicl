;;;; with-array-pointers.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:magicl.cffi-types)

;;; This file contains implementation-specific functions to get
;;; foreign-compatible pointers to the underlying storage of a Lisp
;;; SIMPLE-ARRAY.
;;;
;;; Currently only SBCL, CCL, and ECL are supported.

(declaim (inline array-pointer))

(defun array-pointer (array)
  "Return a foreign pointer to the data stored in the SIMPLE-ARRAY ARRAY."
  (declare (ignorable array)
           (type simple-array array))
  #+sbcl
  (sb-sys:vector-sap (sb-ext:array-storage-vector array))
  #-(or sbcl)
  (error "ARRAY-POINTER not implemented."))

(defmacro with-array-pointers (bindings &body body)
  "Like LET, but binds with dynamic extent the pointers to the data of numeric simple arrays being bound.

WARNING: Do not close over these pointers or otherwise store them outside of the extent of this macro."
  (assert (cl:every (lambda (binding)
                   (and (listp binding)
                        (= 2 (length binding))
                        (symbolp (first binding))))
                 bindings)
          (bindings)
          "Malformed bindings in WITH-ARRAY-POINTERS. Given ~S" bindings)
  #- (or sbcl ccl ecl allegro)
  (error "WITH-ARRAY-POINTERS unsupported on ~A" (lisp-implementation-type))

  (let* ((symbols (mapcar #'first bindings))
         (forms (mapcar #'second bindings))
         (evaled-symbols (mapcar (alexandria:compose #'gensym #'symbol-name) symbols)))
    (if (null bindings)
        `(progn ,@body)
        `(let ,(mapcar #'list evaled-symbols forms)
           #+sbcl
           (sb-sys:with-pinned-objects (,@(loop :for sym :in evaled-symbols
                                                :collect `(sb-ext:array-storage-vector ,sym)))
             (let ,(loop :for s :in symbols
                         :for e :in evaled-symbols
                         :collect `(,s (array-pointer ,e)))
               (declare (type sb-sys:system-area-pointer ,@symbols)
                        #+#:ignore (dynamic-extent ,@symbols))
               ,@body))

           #+ccl
           (progn
             ;; Below is code that is essentially a pluralized form of
             ;; CCL:WITH-POINTER-TO-IVECTOR. We avoid that macro
             ;; because we want to just freeze GCing once.
             ;;
             ;; Check the types of each of the evaluated forms. In the
             ;; parlance of CCL, they should all be IVECTORs.
             ,@(loop :for e :in evaled-symbols
                     :collect `(check-type ,e ccl::ivector))
             ;; We have to stop GCing so as to not let the pointers to
             ;; the IVECTORs move.
             (ccl::without-gcing
               ;; Allocate the pointers (called MACPTRs in CCL).
               (ccl:with-macptrs ,(mapcar #'list symbols)
                 ;; Deposit the addresses of the vector data into the
                 ;; pointers.
                 ,@(loop :for s :in symbols
                         :for e :in evaled-symbols
                         :append (list `(ccl::%vect-data-to-macptr ,e ,s)
                                       ;; CCL bug: pointers to arrays
                                       ;; of complex double floats are
                                       ;; off by 8 bytes.
                                       `(when (equal '(complex double-float)
                                                     (array-element-type ,e))
                                          (ccl:%incf-ptr ,s 8))))
                 ,@body)))

           #+ecl
           (progn
             (let ,(loop :for s :in symbols
                         :for e :in evaled-symbols
                         :collect `(,s (si:make-foreign-data-from-array ,e)))
               ,@body))
           #+allegro
           (let ,(mapcar #'list symbols evaled-symbols)
             ,@body)))))
