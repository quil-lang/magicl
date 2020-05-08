;;;; constructor-tests.lisp
;;;;
;;;; Author: Cole Scott

(in-package #:magicl-tests)

(deftest test-from-array ()
  (labels ((generate-list (dims &optional (offset 0) (type 'double-float))
             (if (null (cdr dims))
                 (loop :for i :from offset :below (+ offset (car dims)) :collect (coerce i type))
                 (let ((offset-step (reduce #'* (cdr dims))))
                   (loop :for i :from 1 :to (car dims)
                         :collect (generate-list (cdr dims) (+ offset (* (1- i) offset-step))))))))
    (loop :for dimensions :on '(6 5 4 3 2 1) :do
      (dolist (input-layout '(:row-major :column-major))
        (dolist (layout '(:row-major :column-major))
          ;; Test n-dims to n-dims
          (let* ((length (reduce #'* dimensions))
                 (array (make-array dimensions :initial-contents (generate-list dimensions) :element-type 'double-float))
                 (tensor (magicl:from-array array dimensions :layout layout :input-layout input-layout)))
            (loop :for i :below length :do
              (is (= i (apply #'magicl:tref tensor (magicl::from-row-major-index i dimensions))))))
          ;; Test 1-dim to n-dims
          (let* ((length (reduce #'* dimensions))
                 (array (make-array length :initial-contents (alexandria:iota length :step 1d0) :element-type 'double-float))
                 (tensor (magicl:from-array array dimensions :layout layout :input-layout input-layout)))
            (let ((index-function
                    (if (eq input-layout ':row-major)
                        #'magicl::from-row-major-index
                        #'magicl::from-column-major-index)))
              (loop :for i :below length :do
                (is (= i (apply #'magicl:tref tensor (funcall index-function i dimensions))))))))))))
