;;; allocation-tests.lisp
;;;
;;; Author: Erik Davis

(in-package #:magicl-tests)

(defstruct alloc-counts
  (calls 0)
  (finals 0))


(deftest test-custom-allocation ()
  (let ((calls 0)
        (finals 0))
    (flet ((alloc (size element-type initial-element)
             (declare (ignore initial-element))
             (let ((storage
                     (make-array size :element-type element-type)))
               (incf calls)
               (values storage
                       (lambda ()
                         (incf finals))))))
      (let ((magicl::*default-allocator* #'alloc))      
        (let ((m1 (magicl:zeros '(2)))
              (m2 (magicl:zeros '(2 3)))
              (m3 (magicl:zeros '(2 3 4))))
          (magicl:copy-tensor m1)
          (magicl:copy-tensor m2)
          (magicl:copy-tensor m3)
          nil)))
    (tg:gc :full t)
    ;; no good way to actually force finalization, so we don't test it
    (is (= 6 calls))))
