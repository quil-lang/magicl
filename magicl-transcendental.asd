;;;; magicl-transcendental.asd
;;;;
;;;; Author: Robert Smith

;;; Adapted from commonqt's qt.asd
(defclass f->so (asdf:source-file)
  ()
  (:default-initargs
   :type "f"))

(defmethod output-files ((operation compile-op) (component f->so))
  (values (list (make-pathname :name "libexpokit"
                               :type #-darwin "so" #+darwin "dylib"
                               :defaults (component-pathname component)))
          t))

(defmethod perform ((operation load-op) (component f->so))
  t)

(defmethod perform ((operation compile-op) (component f->so))
  (flet ((nn (x) (uiop:native-namestring x)))
    (let* ((fortran-file (component-pathname component))
           (object-file (make-pathname :type "o" :defaults fortran-file))
           (shared-object (make-pathname :type #+darwin "dylib" #-darwin "so"
                                         :name "libexpokit"
                                         :defaults fortran-file)))
      (uiop:run-program
       (list "gfortran" "-fPIC" "-c"
             (nn fortran-file)
             "-o"
             (nn object-file)))
      (uiop:run-program
       (list "gfortran" #+darwin "-dynamiclib" #-darwin "-shared"
             "-o" (nn shared-object)
             (nn object-file)
             #+darwin "-lblas"
             #+darwin "-llapack")))))


(asdf:defsystem #:magicl-transcendental
  :license "BSD 3-Clause (See LICENSE.txt)"
  :maintainer "Rigetti Computing"
  :version (:read-file-form "VERSION.txt")
  :depends-on (#:alexandria
               #:cffi
               #:cffi-libffi
               #:magicl)
  :in-order-to ((asdf:test-op (asdf:test-op #:magicl-tests)))
  :serial t
  :components
  ((:module "transcendental"
    :serial t
    :components ((f->so "expokit")
                 (:file "package")
                 (:file "load-libs")
                 (:file "transcendental")))
   (:file "expokit-cffi")))

