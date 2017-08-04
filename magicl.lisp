(in-package #:magicl)

#+sbcl
(defmacro with-blapack (&body body)
  `(sb-int:with-float-traps-masked (:divide-by-zero)
    ,@body))

#+cmu
(defmacro with-blapack (&body body)
  `(extensions:with-float-traps-masked (:divide-by-zero)
     ,@body))

#-(or sbcl cmu)
(defmacro with-blapack (&body body)
  `(progn
    ,@body))
