;;;; lapack-bindings.lisp
;;;;
;;;; Author: Cole Scott

(in-package #:magicl-lapack)

(macrolet ((def-all-lapack ()
             "Define the lapack bindings for all lapack types"
             `(progn
                ,@(loop :for type :in '(single-float double-float (complex single-float) (complex double-float))
                        :for real-type :in (list nil nil 'single-float 'double-float)
                        :for matrix-class :in '(matrix/single-float matrix/double-float matrix/complex-single-float matrix/complex-double-float)
                        :for vector-class :in '(vector/single-float vector/double-float vector/complex-single-float vector/complex-double-float)
                        :for prefix :in '("s" "d" "c" "z")
                        :append
                        (labels ((generate-routine-symbol (package routine)
                                   (find-symbol (format nil "%~:@(~A~A~)" prefix routine) package))
                                 (blas-routine (routine)
                                   (generate-routine-symbol 'magicl.blas-cffi routine))
                                 (lapack-routine (routine)
                                   (generate-routine-symbol 'magicl.lapack-cffi routine)))
                          (let ((complex (not (null real-type))))
                            (list
                             (magicl::generate-lapack-mult-for-type
                              'mult-extension
                              matrix-class vector-class type
                              (blas-routine "gemm") (blas-routine "gemv"))
                             (magicl::generate-lapack-lu-for-type
                              'lapack-lu
                              matrix-class type (lapack-routine "getrf"))
                             (generate-lapack-inv-for-type
                              matrix-class type
                              (lapack-routine "getrf") (lapack-routine "getri"))
                             (magicl::generate-lapack-svd-for-type
                              'lapack-svd
                              matrix-class type
                              (lapack-routine "gesvd")
                              real-type)
                             (magicl::generate-lapack-eig-for-type
                              'lapack-eig
                              matrix-class type
                              (lapack-routine "geev")
                              real-type)
                             (generate-lapack-ql-qr-rq-lq-for-type
                              matrix-class type
                              (lapack-routine "geqlf") (lapack-routine "geqrf")
                              (lapack-routine "gerqf") (lapack-routine "gelqf")
                              (lapack-routine (if complex "ungql" "orgql"))
                              (lapack-routine (if complex "ungqr" "orgqr"))
                              (lapack-routine (if complex "ungrq" "orgrq"))
                              (lapack-routine (if complex "unglq" "orglq")))
                             (when complex
                               (magicl::generate-lapack-hermitian-eig-for-type
                                'lapack-hermitian-eig
                                matrix-class type
                                (lapack-routine "heev")
                                real-type)))))))))
  (def-all-lapack))
