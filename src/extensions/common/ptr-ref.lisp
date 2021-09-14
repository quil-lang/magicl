(in-package #:magicl.cffi-types)

;; TODO: This should be generic to abstract-tensor
(defgeneric ptr-ref (m base i j)
  (:documentation
   "Accessor method for the pointer to the element in the I-th row and J-th column of a matrix M, assuming zero indexing.")
  (:method ((m magicl:matrix) base i j)
    (policy-cond:with-expectations (> speed safety)
        ((assertion (magicl:valid-index-p (list i j) (magicl:shape m))))
      (let ((type (magicl:element-type m)))
        (let ((idx (apply (ecase (magicl:layout m)
                            (:column-major #'magicl:matrix-column-major-index)
                            (:row-major #'magicl:matrix-row-major-index))
                          i j (magicl:shape m))))
          (cond
            ((subtypep type 'single-float) (cffi:mem-aptr base :float idx))
            ((subtypep type 'double-float) (cffi:mem-aptr base :double idx))
            ((subtypep type '(complex single-float)) (cffi:mem-aptr base :float (* 2 idx)))
            ((subtypep type '(complex double-float)) (cffi:mem-aptr base :double (* 2 idx)))
            (t (error "Incompatible element type ~a." type))))))))

