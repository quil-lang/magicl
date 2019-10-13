;;;; abstract-tensor-tests.lisp
;;;;
;;;; Author: Cole Scott

(in-package #:magicl-tests)

(defun from-row-major-index (index dims)
  (check-type index fixnum)
  (check-type dims shape)
  (reverse (loop :for d in dims
                 :for acc = index :then (floor acc d)
                 :collect (rem acc d))))

(deftest test-tensor-shape ()
  "Test that the shape is returned correctly for tensors of dimension 1 to 8"
  (let ((dimensions (make-list 1 :initial-element 1)))
    (loop :for rank :from 2 :to 8
          :do (rplacd (last dimensions) (list rank))
              (is (equal dimensions (shape (magicl:empty dimensions)))))))

(deftest test-tensor-rank ()
  "Test that the rank is returned correctly for tensors of dimension 1 to 8"
  (let ((dimensions (make-list 1 :initial-element 1)))
    (loop :for rank :from 2 :to 8
          :do (rplacd (last dimensions) (list rank))
              (is (equal rank (rank (magicl:empty dimensions)))))))

(deftest test-tensor-tref ()
  (let ((tensor (magicl:from-list '(1  2  3  4  5
                                    6  7  8  9  10
                                    11 12 13 14 15)
                                  '(3 5)
                                  :type 'double-float)))
    (loop :for i :below 15
          :do (format t "~a~%" (from-row-major-index i '(3 5))))))
