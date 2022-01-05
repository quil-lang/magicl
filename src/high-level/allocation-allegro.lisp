;;;; allocation-allegro.lisp
;;;; special implementation of `lisp-allocator' function for allegro
;;;; it is loaded AFTER `allocation.lisp' file
;;;;
;;;; Author: Tianyu Gu

(in-package #:magicl)

(defun lisp-allocator (size element-type initial-element)
  (let ((storage (let ((init-element (if initial-element (coerce initial-element element-type))))
                   (cond ((eq element-type 'single-float)
                          (excl::.primcall 'sys::make-svector size init-element #x71 initial-element nil))
                         ((eq element-type 'double-float)
                          (excl::.primcall 'sys::make-svector size init-element #x72 initial-element nil))
                         ((equal element-type '(complex single-float))
                          (excl::.primcall 'sys::make-svector size init-element #x73 initial-element nil))
                         ((equal element-type '(complex double-float))
                          (excl::.primcall 'sys::make-svector size init-element #x74 initial-element nil))
                         ((equal element-type '(signed-byte 32))
                          (excl::.primcall 'sys::make-svector size init-element #x7b initial-element nil))
                         (t (apply #'make-array
                                   size
                                   :element-type element-type
                                   (if initial-element
                                       (list ':initial-element (coerce initial-element element-type))
                                       nil)))))))
    (values storage #'dummy-finalizer)))
