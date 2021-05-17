(in-package #:magicl.foreign-libraries)

(defvar *foreign-libraries* nil
  "Foreign library names (designated by symbols) that exist.

Each new MAGICL extension is expected to PUSHNEW the library symbols to this variable.")

(defun foreign-symbol-available-p (name library)
  "Check that NAME is available from the libarary LIBRARY."
  (check-type name string)
  (check-type library symbol)
  (let ((found (cffi:foreign-symbol-pointer name :library library)))
    (and found
         (not (cffi:null-pointer-p found))
         t)))

(defun track-symbols (library symbol-quadruples)
  (dolist (quad symbol-quadruples)
    (pushnew quad (getf (symbol-plist library) ':magicl)
             :test #'string=
             :key #'first)))

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
  (dolist (lib (sort (remove-duplicates (copy-list *foreign-libraries*)) #'string<))
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
