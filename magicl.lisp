(in-package #:magicl)

#+sbcl
(defmacro with-blapack (&body body)
  `(sb-int:with-float-traps-masked (:divide-by-zero :invalid)
    ,@body))

#+cmu
(defmacro with-blapack (&body body)
  `(extensions:with-float-traps-masked (:divide-by-zero)
     ,@body))

#+ccl
(defmacro with-blapack (&body body)
  (alexandria:with-gensyms (fpu-mode)
    `(let ((,fpu-mode (ccl:get-fpu-mode)))
       (unwind-protect (progn
                         (ccl:set-fpu-mode :division-by-zero nil
                                           :invalid nil)
                         ,@body)
         (apply 'ccl:set-fpu-mode ,fpu-mode)))))

#-(or sbcl cmu ccl)
(defmacro with-blapack (&body body)
  `(progn
    ,@body))
