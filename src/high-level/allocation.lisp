;;; allocation.lisp
;;;
;;; Control allocation of tensors. Derived from QVM's allocator.lisp
;;; 
;;; Author: Erik Davis

(in-package #:magicl)


(deftype finalizer ()
  "A finalizer thunk. Used for the effect of freeing some memory."
  '(function () null))

(defun dummy-finalizer ()
  "A \"finalizer\" that does nothing. Used for objects managed by the GC."
  nil)

(deftype allocator ()
  "A routine to allocate storage for a fresh tensor with an indicated number of elements of type ELEMENT-TYPE, with entries defaulting to INITIAL-ELEMENT. Return two values:
    1. The allocated storage.
    2. A finalizer thunk of type FINALIZER, which should be called when the memory is OK to be freed.
NOTE: Note that the finalizer may close over the allocated vector."
  '(function (integer &key (:element-type t) (:initial-element t)) (values (simple-array *) finalizer)))


;;;;;;;;;;;;;;;;;;;;;;;; Lisp Heap Allocation ;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (ftype allocator lisp-allocator))
(defun lisp-allocator (size &key element-type initial-element)
  (let ((storage
          (apply #'make-array
                 size
                 :element-type element-type
                 (if initial-element
                     (list ':initial-element (coerce initial-element element-type))
                     nil))))
    (values storage
            #'dummy-finalizer)))


;;;;;;;;;;;;;;;;;;;;; Foreign Memory Allocation ;;;;;;;;;;;;;;;;;;;;;;

(declaim (ftype allocator c-allocator))
(defun c-allocator (size &key element-type initial-element)
  (let ((storage
          (apply #'static-vectors:make-static-vector
                 size
                 :element-type element-type
                 (if initial-element
                     (list ':initial-element (coerce initial-element element-type))
                     nil))))
    (values storage
            (lambda ()
              (static-vectors:free-static-vector storage)))))


;;;;;;;;;;;;;;;;;;;;; Default Allocation Settings ;;;;;;;;;;;;;;;;;;;;;;

(declaim (type allocator *default-allocator*))
(defvar *default-allocator* #'lisp-allocator
  "The default allocation to use for tensor storage. ")

(defun allocate (size &key element-type initial-element)
  "Allocate storage for a fresh tensor."
  (funcall *default-allocator* size
           :element-type element-type
           :initial-element initial-element))
