;;;; with-array-pointers.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:magicl.cffi-types)

;;; This file contains implementation-specific functions to get
;;; foreign-compatible pointers to the underlying storage of a Lisp
;;; SIMPLE-ARRAY.
;;;
;;; Currently only SBCL is supported.

(declaim (inline array-pointer))

(defun array-pointer (array)
  "Return a foreign pointer to the data stored in the SIMPLE-ARRAY ARRAY."
  (declare (type simple-array array))
  #+sbcl
  (sb-sys:vector-sap (sb-ext:array-storage-vector array))
  #-(or sbcl)
  (error "ARRAY-POINTER not implemented."))

(defmacro with-array-pointers (bindings &body body)
  "Like LET, but binds with dynamic extent the pointers to the data of simple arrays being bound.

WARNING: Do not close over these pointers or otherwise store them outside of the extent of this macro."
  (assert (every (lambda (binding)
                   (and (listp binding)
                        (= 2 (length binding))
                        (symbolp (first binding))))
                 bindings)
          (bindings)
          "Malformed bindings in WITH-ARRAY-POINTERS. Given ~S" bindings)
  (let* ((symbols (mapcar #'first bindings))
         (forms (mapcar #'second bindings))
         (evaled-symbols (mapcar (alexandria:compose #'gensym #'symbol-name) symbols)))
    (if (null bindings)
        `(progn ,@body)
        `(let ,(mapcar #'list evaled-symbols forms)
           (sb-sys:with-pinned-objects (,@evaled-symbols)
             (let ,(loop :for s :in symbols
                         :for e :in evaled-symbols
                         :collect `(,s (array-pointer ,e)))
               (declare (type sb-sys:system-area-pointer ,@symbols)
                        #+#:ignore (dynamic-extent ,@symbols))
               ,@body))))))
