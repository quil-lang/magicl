(in-package #:magicl.foreign-libraries)

;;; This command is useful for inspecting shared libraries: nm -jgU

(cffi:define-foreign-library libblas
  #+:magicl.use-accelerate
  (:darwin "libBLAS.dylib" :search-path #P"/System/Library/Frameworks/Accelerate.framework/Frameworks/vecLib.framework/Versions/A/")
  #-:magicl.use-accelerate              ; KLUDGE: Look for Homebrew dir first.
  (:darwin (:or "libblas.dylib" "/usr/local/opt/lapack/lib/libblas.dylib"))
  #+:magicl.use-mkl
  (:unix  "libmkl_rt.so")
  #-:magicl.use-mkl
  (:unix  "libblas.so")
  (t (:default "libblas")))

(cffi:define-foreign-library liblapack
  #+:magicl.use-accelerate
  (:darwin "libLAPACK.dylib" :search-path #P"/System/Library/Frameworks/Accelerate.framework/Frameworks/vecLib.framework/Versions/A/")
  #-:magicl.use-accelerate
  (:darwin (:or "liblapack.dylib" "/usr/local/opt/lapack/lib/liblapack.dylib"))
  #+:magicl.use-mkl
  (:unix  "libmkl_rt.so")
  #-:magicl.use-mkl
  (:unix  "liblapack.so")
  (t (:default "liblapack")))

(defvar *cffi-libraries* '(libblas liblapack))

(defun foreign-symbol-available-p (name library)
  "Check that NAME is available from the libarary LIBRARY."
  (check-type name string)
  (check-type library symbol)
  (let ((found (cffi:foreign-symbol-pointer name :library library)))
    (and found
         (not (cffi:null-pointer-p found))
         t)))

(defun print-availability-report (&key (stream *standard-output*)
                                       (show-available t)
                                       (show-unavailable t)
                                       search)
  (check-type search (or null string))
  (check-type stream stream)
  ;; Print header
  (format stream "~&~7T~
                  ~T~A~
                  ~0,32T~A~%"
          "Fortran Function"
          "Lisp Function")
  (write-line #.(make-string 72 :initial-element #\-) stream)
  (terpri stream)

  ;; Print symbol information
  (dolist (lib *cffi-libraries*)
    (let ((symbols (sort (copy-seq (getf (symbol-plist lib) ':magicl))
                         #'string<
                         :key #'first)))
      (unless (null symbols)
        (format stream "Library ~A: ~A~%"
                (symbol-name lib)
                (cffi:foreign-library-pathname lib))
        
        (loop :for (real-name mangled-name raw-symbol external-symbol) :in symbols :do
          (let ((available (foreign-symbol-available-p mangled-name lib)))
            (when (and
                   ;; search query
                   (or (null search)
                       (search search real-name :test #'char-equal)
                       (search search mangled-name :test #'char-equal))
                   ;; availability filter
                   (or (and show-available available)
                       (and show-unavailable (not available))))
              (format stream "~4T[~:[ ~;x~]]~
                              ~T~A~
                              ~0,32T~S~%"
                      available
                      real-name
                      external-symbol))))
        (terpri stream)))))

;;;;;;;;;;;;;;;;;;;;;;;;; Load the Libraies ;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *blapack-libs-loaded* nil)

(unless *blapack-libs-loaded*
  (cffi:load-foreign-library 'libblas)
  (cffi:load-foreign-library 'liblapack)
  (setf *blapack-libs-loaded* t))
