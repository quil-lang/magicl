(defpackage #:magicl.generate-interface
  (:use #:common-lisp
        #:magicl.cffi-types)
  (:export #:*inclusion-criterion*
           #:generate-blas-files
           #:generate-lapack-files
           #:generate-lapack-files*
           #:generate-expokit-files))

(in-package #:magicl.generate-interface)

;;; We maximize safety because this is mostly generating and writing
;;; out code.
(declaim (optimize (safety 3) (debug 3) (speed 1)))

(defun desired-function-p (string)
  (let ((desired-functions
          ;; We only want to do this once when this file is being
          ;; compiled.
          '#.(let ((file (asdf:system-relative-pathname "magicl" "src/functions.txt")))
               (with-open-file (s file :direction ':input :if-does-not-exist ':error)
                 (loop :for line := (read-line s nil nil)
                       :while line
                       :unless (or (zerop (length line))
                                   (eql 0 (position #\; line)))
                         :collect line :into lines
                       :finally (return (sort lines #'string<)))))))
    (find string
          desired-functions
          :test (lambda (haystack needle)
                  (search needle haystack :test #'char-equal)))))

(defparameter *inclusion-criterion* 'desired-function-p
  "A function designator which determines if a BLAS or LAPACK function
will be included. Should take the name of a Fortran function, such as
\"dgges\", and return a boolean as to whether it should be generated.

By default this is bound to the symbol DESIRED-FUNCTION-P, which will
check the functions.txt list for valid functions.

It may be bound to (CONSTANTLY T) to include everything.")

(defun read-lines (file)
  "Returns a list of strings, one string per line of file."
  (with-open-file (f file :direction :input)
    (loop :for line := (read-line f nil) :when line :collect line :while line)))

(defun comment-line-p (line)
  (member (first line) (list "c" "*") :test #'string=))

(defun to-string (char-list)
  (map 'string #'identity char-list))


(defun tokenize (line)
  "Tokenizes a line.  Removes spaces and commas."
  (let ((tokens nil)
	(cur-token nil))
    (map nil
	 (lambda (c)
	   (cond
	     ((member c '(#\Space #\,))
	      (when cur-token
		(push (to-string (nreverse cur-token)) tokens)
		(setf cur-token nil)))
	     ((member c '(#\( #\)))
	      (when cur-token
		(push (to-string (nreverse cur-token)) tokens))
	      (push (to-string (list c)) tokens)
	      (setf cur-token nil))
	     (t (push c cur-token))))
	 line)
    (when cur-token
      (push (to-string (nreverse cur-token)) tokens))
    (nreverse tokens)))

(defparameter *lines* nil
  "This special variable holds the lines of the file currently being
  parsed.  It's a kludge that makes life easy.")

(defparameter *types* '(("character") ("character*1") ("character*6")
			("character*")
			("integer") ("real")
			("complex") ("double" "precision")
			("double" "complex") ("complex*16")
			("logical")))

(defparameter *typemap*
  '((("character")          :fortran-string)
    (("character*")         :fortran-string)
    (("character*1")        :fortran-string)
    (("character*6")        :fortran-string)
    (("integer")            :fortran-int)
    (("real")               :fortran-single-float)
    (("double" "precision") :fortran-double-float)
    (("complex")            :fortran-complex-single-float)
    (("double" "complex")   :fortran-complex-double-float)
    (("complex*16")         :fortran-complex-double-float)
    (("logical")            :fortran-logical)
    (("none")               :fortran-none)))

(defparameter *normalized-type-to-cffi-type*
  ;; normalized type               reference CFFI type     immediate CFFI type   lisp type
  '((:fortran-string               :string                 :string               string)
    (:fortran-int                  fortran-int             :int32                (signed-byte 32))
    (:fortran-single-float         fortran-float           :float                single-float)
    (:fortran-double-float         fortran-double          :double               double-float)
    (:fortran-complex-single-float fortran-complex-float   complex-single-float  (complex single-float))
    (:fortran-complex-double-float fortran-complex-double  complex-double-float  (complex double-float))
    (:fortran-logical              fortran-logical         :int32                (signed-byte 32))
    (:fortran-none                 :void                   :void                 nil)))

(defparameter *array-of-normalized-type-to-cffi-type*
  '((:fortran-string               :pointer array)
    (:fortran-int                  :pointer (simple-array (signed-byte 32) (*)))
    (:fortran-single-float         :pointer (simple-array single-float) (*))
    (:fortran-double-float         :pointer (simple-array double-float) (*))
    (:fortran-complex-single-float :pointer (simple-array (complex single-float) (*)))
    (:fortran-complex-double-float :pointer (simple-array (complex double-float) (*)))
    (:fortran-logical              :pointer (simple-array (signed-byte 32) (*)))
    (:fortran-none                 :pointer array)))

(defun normalized-type-to-cffi-type (norm &optional (kind ':reference))
  (etypecase norm
    (keyword
     (let ((found (assoc norm *normalized-type-to-cffi-type*)))
       (assert found)
       (ecase kind
         (:reference (second found))
         (:immediate (third found))
         (:lisp      (fourth found)))))

    (list
     (destructuring-bind (compound-type base-type rank)
         norm
       (assert (eq ':fortran-array compound-type))
       (assert (and (integerp rank)
                    (<= 1 rank)))
       (let ((found (assoc base-type *array-of-normalized-type-to-cffi-type*)))
         (assert found)
         (ecase kind
           ((:reference :immediate) (second found))
           (:lisp (third found))))))))

(defparameter *ctype-to-fortrantype*
  '((:string        :string)
    (:int32         fortran-int)
    (:float         fortran-float)
    (:double        fortran-double)
    (complex-float  fortran-complex-float)
    (complex-double fortran-complex-double)
    (:logical       fortran-logical)))

(defun cffi-type-to-fortran-type (cffi-type)
  (cadr (assoc cffi-type *ctype-to-fortrantype*)))

(defun extract-continued-line ()
  "Fortran can't have long lines, and so will use a $ or . in column 7 to
indicate a continued line.  We'll pop off these $'s and .'s and put them
together into one line, and return that extended line and the remainder."
  (let ((line (pop *lines*)))
    (loop while (and (>= (length (first *lines*)) 6)
                     (or
                      (char= (char (first *lines*) 5) #\$)
                      (char= (char (first *lines*) 5) #\.))) do
          (setf line (concatenate 'string line (subseq (pop *lines*) 6))))
    (tokenize line)))

(defun parse-signature ()
  "Parses the name of the function and the names of all the arguments.
Returns three values: the name, the list of arguments, and all
remaining lines."
  (let* ((line (extract-continued-line))
         (subroutine-line (cdr (member "subroutine" line :test #'string-equal)))
         (function-line (cdr (member "function" line :test #'string-equal)))
         (return-type (list "none"))
         signature name vars)
     (if subroutine-line
        (setf signature subroutine-line)
        (when function-line
            (setf signature function-line
                  return-type (subseq line 0 (- (length line) (length function-line) 1)))))
    (if signature
	(setf name (car signature)
	      vars (subseq signature 2 (- (length signature) 1)))
        (error "Can't parse routine: ~A | ~A" (car line) line))
    (values name vars return-type)))

(defun extract-type (line)
  (let ((type
	 (find-if (lambda (e)
		    (every #'string-equal e (subseq line 0 (length e))))
		  *types*))
        args)
    (unless type
      (error "Can't find type: ~A" line))
    (setf args (subseq line (length type)))
    (when (every #'(lambda (x) (char-equal #\* (char x (- (length x) 1)))) args)
      (setf args (map 'list #'(lambda (x) (string-right-trim "*" x)) args))
      (setf type (list (concatenate 'string (first type) "*")))) ; TODO: modify this in case some but not all args in the line have stars
    (values type args)))

(defun fill-in-type (names type vars array-maps)
  (mapcar (lambda (v)
	    (let ((pair (assoc v names :test #'string-equal)))
	      (when pair
		(setf (cdr pair)
		      (append type
			      (cdr (assoc v array-maps :test #'string-equal)))))))
	  vars))


(defun deparenthesize (line)
  "Get rid of stupid parentheses around array arguments."
  (let ((keepers nil)
	(array-maps nil)
	(in-array nil)
	(prev nil))
    (loop for i in line do
	  (cond ((string-equal i "(" )
		 (push prev in-array))
		((string-equal i ")" )
		 (progn
		   (push (nreverse in-array) array-maps)
		   (setf in-array nil)))
		(in-array
		 (push i in-array))
		(t
		 (push i keepers)))
	  (setf prev i))
    (values (nreverse keepers) (nreverse array-maps))))


(defun parse-argument-types (names)
  (let ((names (mapcar (lambda (n) (cons n nil)) names)))
    (loop while (find-if #'null names :key #'cdr) do
	  (let ((line (extract-continued-line)))
     	    (when (not (comment-line-p line))
	      (multiple-value-bind (line array-maps)
		  (deparenthesize line)
                (when (variable-declaration-p line)
                  (multiple-value-bind (type vars)
                      (extract-type line)
                    (fill-in-type names type vars array-maps)))))))
    names))

(defun variable-declaration-p (line)
  (let ((one-word-type (list (string-downcase (first line))))
        (two-word-type (if (rest line) (map 'list #'string-downcase (subseq line 0 2)))))
    (or (member one-word-type *types* :test 'equal) (member two-word-type *types* :test 'equal))))

(defstruct fortran-function
  name
  return-type
  arguments)

(defun lisp-fun-name (ff)
  "The name of the normal Lisp entry point to the Fortran function FF."
  (intern (concatenate 'string "%" (string-upcase (fortran-function-name ff))) *package*))

(defun raw-call-name (ff)
  "The name of the raw CFFI-defined function for the Fortran function FF."
  (intern (concatenate 'string "%%" (string-upcase (fortran-function-name ff))) *package*))

(defun parse-fortran-file (fortran-file)
  (format *trace-output* "; Reading Fortran file: ~A~%" fortran-file)
  (finish-output *trace-output*)
  ;; Collect the lines
  (setf *lines* (read-lines fortran-file))
  ;; Remove comments
  (setf *lines* (delete-if (lambda (line)
                             (or (zerop (length line))
                                 (char= #\* (char line 0))
                                 (char= #\c (char line 0))))
                           *lines*))
  (multiple-value-bind (name vars return-type)
      (parse-signature)
    (make-fortran-function
     :name name
     :return-type (lookup-type return-type)
     ;; Normalize the types.
     :arguments (mapcar (lambda (name-type)
                          (list (first name-type)
                                (lookup-type (rest name-type))))
                        (parse-argument-types vars)))))

(defvar *basedir*)
(defparameter *outdir*
  (asdf:system-relative-pathname :magicl-gen #+allegro "src/bindings/allegro/"
                                             #-allegro "src/bindings/"))

(defun parse-blas-files (&optional (basedir *basedir*))
  (let ((files
	  (directory
	     (pathname
	      (concatenate 'string
			   (namestring basedir) "BLAS/SRC/*.f")))))
    (loop :for file :in files
          :when (funcall *inclusion-criterion* (pathname-name file))
            :collect (parse-fortran-file file))))

(defun parse-lapack-files (&optional (basedir *basedir*))
  (let ((files
	 (directory
	  (pathname
	   (concatenate 'string (namestring basedir) "SRC/*.f")))))
    (loop :for file :in files
          :when (funcall *inclusion-criterion* (pathname-name file))
            :collect (parse-fortran-file file))))

(defun make-lapack-parser* (&optional (basedir *basedir*) (chunks 8))
  (let* ((files
           (directory
            (pathname
             (concatenate 'string (namestring basedir) "SRC/*.f"))))
         (chunk-size (ceiling (length files) chunks)))
    (format *trace-output* "; Creating generator with batch size: ~s~%" chunk-size)
    (lambda ()
      (loop
         :repeat chunk-size
         :for file := (pop files)
         :while file
         :when (funcall *inclusion-criterion* (pathname-name file))
           :collect (parse-fortran-file file)))))

(defun lookup-type (type-string-list)
  (let ((found-type nil)
        (match-length 0))
    (loop :for (fortran-type-tokens normalized-type) :in *typemap*
          :for mismatch := (or (mismatch type-string-list fortran-type-tokens :test #'string-equal)
                               (length type-string-list))
          :when (> mismatch match-length) :do
            (setf match-length mismatch
                  found-type normalized-type))
    (assert found-type () "Didn't find a type for ~S" type-string-list)
    (let ((rest (subseq type-string-list match-length)))
      (if (null rest)
          found-type
          `(:fortran-array ,found-type ,(length rest))))))


(defun fortran-mangle-name (name)
  "Turns a fortran library function into a C library function.  May
need to be customized."
  (concatenate 'string (string-downcase name) "_"))

(defun generate-cffi-interface (parsed-representation)
  (let ((name (fortran-function-name parsed-representation))
        (vars (fortran-function-arguments parsed-representation))
        (return-type (fortran-function-return-type parsed-representation)))
    `(cffi:defcfun
         ;; (name lisp-name)
         (,(fortran-mangle-name name) ,(intern
                                        (concatenate 'string "%" (string-upcase name))))
         ;; return type
         ,(normalized-type-to-cffi-type return-type ':immediate)
       ;; params and their types
       ,@(mapcar (lambda (v)
                   (list (intern (string-upcase (first v)) *package*)
                         (normalized-type-to-cffi-type (second v))))
                 vars))))

(defun generate-cffi-interface-alternate (ff &key originating-library)
  (labels ((sym (var-name)
             (intern (string-upcase var-name) *package*)))
    (let* ((name (fortran-function-name ff))
           (fortran-name (fortran-mangle-name name))
           (vars-types (fortran-function-arguments ff))
           (vars (mapcar (lambda (vt) (sym (first vt))) vars-types))
           (ref-vars (mapcar (lambda (v) (gentemp (concatenate 'string (symbol-name v) "-REF")
                                                  *package*))
                             vars))
           (normalized-types (mapcar #'second vars-types))
           (return-type (fortran-function-return-type ff))
           (raw-call-name (raw-call-name ff))
           (lisp-fun-name (lisp-fun-name ff)))
      `(
        ;; Allegro FFI form
        #+allegro
        (ff:def-foreign-call (,raw-call-name ,fortran-name)
            ,(mapcar (lambda (var norm-type)
                       `(,(sym var)
                         ,@(if (eq norm-type :fortran-string) '((* :char) string) '(:foreign-address))))
                     vars normalized-types)
          :returning ,(ecase return-type
                        (:fortran-string '((* :char) string))
                        (:fortran-int :int)
                        (:fortran-single-float '(:float single-float))
                        (:fortran-double-float '(:double double-float))
                        (:fortran-complex-single-float '(:foreign-address (complex single-float)))
                        (:fortran-complex-double-float '(:foreign-address (complex double-float)))
                        (:fortran-logical '(:int (signed-byte 32)))
                        (:fortran-none :void))
          :release-heap :never
          :allow-gc :never)

        ;; CFFI form
        #-allegro
        (cffi:defcfun (,fortran-name ,raw-call-name
                       ,@(if (not originating-library)
                             nil
                             `(:library ,originating-library)))
            ,(normalized-type-to-cffi-type return-type ':immediate)
          ,@(loop :for var :in vars
                  :for norm-type :in normalized-types
                  ;; TODO: add more type information
                  :collect (list (sym var) (if (eq norm-type ':fortran-string)
                                               ':string
                                               ':pointer))))

        ;; Lisp function form
        (cl:defun ,lisp-fun-name ,(mapcar #'sym vars)
          ;; Inline and type declaration expressions
          (cl:declare (cl:inline ,raw-call-name)
                      ,@(loop :for var :in vars
                              :for norm-type :in normalized-types
                              :collect `(cl:type ,(normalized-type-to-cffi-type
                                                   norm-type
                                                   ':lisp)
                                                 ,var )))
          ;; Foreign allocation of single-element references.
          (cffi:with-foreign-objects ,(loop :for ref-var :in ref-vars
                                            :for norm-type :in normalized-types
                                            :when (and (atom norm-type)
                                                       (not (eq ':fortran-string norm-type)))
                                              :collect `(,ref-var
                                                         ',(normalized-type-to-cffi-type norm-type ':immediate)))
            ;; Setters for single-item references.
            ,@(remove nil
                      (loop :with real := 'magicl.cffi-types::real
                            :with imag := 'magicl.cffi-types::imag
                            :for var :in vars
                            :for ref-var :in ref-vars
                            :for norm-type :in normalized-types
                            :when (atom norm-type)
                              :collect (case norm-type
                                         (:fortran-string
                                          ;; No copying needed.
                                          nil)

                                         ((:fortran-int
                                           :fortran-single-float
                                           :fortran-double-float
                                           :fortran-logical)
                                          ;; Immediate set
                                          `(cl:setf (cffi:mem-ref
                                                     ,ref-var
                                                     ,(normalized-type-to-cffi-type
                                                       norm-type
                                                       ':immediate))
                                                    ,var))
                                         ((:fortran-complex-single-float
                                           :fortran-complex-double-float)
                                          `(cffi:with-foreign-slots ((,real ,imag)
                                                                     ,ref-var
                                                                     ,(normalized-type-to-cffi-type norm-type ':immediate))
                                             (setf ,real (realpart ,var)
                                                   ,imag (imagpart ,var))))

                                         (otherwise
                                          (error "Invalid argument type: ~S" norm-type)))))
            ;; Extraction of array pointers for array arguments.
            (magicl.cffi-types:with-array-pointers
                ,(loop :for var :in vars
                       :for ref-var :in ref-vars
                       :for norm-type :in normalized-types
                       :unless (atom norm-type)
                         :collect `(,ref-var ,var))

                ;; The raw call.
                (,raw-call-name
                 ,@(loop :for var :in vars
                         :for ref-var :in ref-vars
                         :for norm-type :in normalized-types
                         :collect (cond
                                    ((eq ':fortran-string norm-type) var)
                                    ((atom norm-type) ref-var)
                                    (t ref-var)))))))))))

(defun generate-bindings-file (filename package-name bindings
			       &optional (outdir *outdir*))
  "This does the bulk of the work in getting things automagically
done, and is used by generate-blas-bindings etc to automagically do
the CFFI binding file."
  (let ((*print-pretty* t))
    (with-open-file (f (make-pathname :name filename
                                      :type "lisp"
                                      :defaults outdir)
                       :direction :output
                       :if-exists :supersede)
      ;; HEADER
      ;;
      ;; Print the time.
      (multiple-value-bind
            (second minute hour date month year day-of-week dst-p tz)
          (get-decoded-time)
        (declare (ignore day-of-week dst-p))
        (format f ";;;; Generated on ~d/~2,'0d/~d at ~2,'0d:~2,'0d:~2,'0d (UTC~@d)."
                month
                date
                year
                hour
                minute
                second
                (- tz)))
      ;; Print the package form.
      (terpri f)
      (terpri f)
      (prin1 `(declaim (optimize (speed 0) safety debug compilation-speed)) f)
      (terpri f)
      (terpri f)
      (prin1 `(in-package ,package-name) f)
      (terpri f)
      (terpri f)
      ;; Print the bindings.
      (dolist (form bindings)
        (cond
          ((eq 'cffi:defcfun (car form))
           (let ((name (cadadr form)))
             (declare (ignore name))
             (prin1 form f)
             (terpri f)
             (terpri f)))
          ((eq 'cl:defun (car form))
           (let ((name (cadr form)))
             (declare (ignore name))
             (prin1 form f)
             (terpri f)
             (terpri f)))
          (t
           (prin1 form f)
           (terpri f)
           (terpri f))))
      (write-line ";;; End of file." f)
      nil)))

(defun generate-file (file-name package-name library-name parsing-function)
  (let* ((*package* (find-package package-name))
         (parsed-ffs (funcall parsing-function))
         (originating-library library-name))
    (generate-bindings-file
     file-name
     package-name
     (append
      ;; Inline decls
      (list
       `(cl:declaim (cl:inline ,@(loop :for ff :in parsed-ffs
                                       :append (list
                                                (raw-call-name ff)
                                                (lisp-fun-name ff))))))
      ;; Function bindings
      (mapcan (lambda (def)
                (generate-cffi-interface-alternate
                 def
                 :originating-library originating-library))
              parsed-ffs)
      ;; Not-Inline decls
      (list
       `(cl:declaim (cl:notinline ,@(loop :for ff :in parsed-ffs
                                          :append (list
                                                   (raw-call-name ff)
                                                   (lisp-fun-name ff))))))
      ;; Record the following in the SYMBOL-PLIST of the library
      ;; symbol:
      ;;
      ;;     1. The function name as it appears in the Fortran file.
      ;;
      ;;     2. The mangled name.
      ;;
      ;;     3. The Lisp symbol name refering to the CFFI-defined
      ;;        function.
      ;;
      ;;     4. The "high level" entry point function which handles
      ;;        the by-ref semantics of Fortran.

      (list
       (let ((entries (loop :for ff :in parsed-ffs
                            :collect (list (fortran-function-name ff)
                                           (fortran-mangle-name
                                            (fortran-function-name ff))
                                           (raw-call-name ff)
                                           (lisp-fun-name ff)))))
         `(magicl.foreign-libraries:track-symbols
           ',originating-library
           ',entries)))

      ;; Exports
      (list
       `(cl:export ',(loop :for ff :in parsed-ffs
                           :collect (lisp-fun-name ff))
                   ',package-name)))

     ;; Specify target directory
     *outdir*)))

(defun generate-blas-file ()
  (generate-file "blas-cffi"
                 '#:magicl.blas-cffi
                 'magicl.foreign-libraries::libblas
                 #'parse-blas-files))

(defun generate-blas-files (lapack-dir)
  (let ((*basedir* lapack-dir))
    (generate-blas-file)))

(defun generate-lapack-files (lapack-dir)
  "Generate all LAPACK bindings in a single file."
  (let ((*basedir* lapack-dir))
    (generate-file "lapack-cffi"
                   '#:magicl.lapack-cffi
                   'magicl.foreign-libraries::liblapack
                   #'parse-lapack-files)))

(defun generate-lapack-files* (lapack-dir)
  (loop
     with *basedir* = lapack-dir
     with number-of-chunks = 8
     with parser = (make-lapack-parser* *basedir* number-of-chunks)
     for i from 0 below number-of-chunks
     for file-name = (format nil "lapack~2,'0d-cffi" i)
     do (generate-file file-name
                       '#:magicl.lapack-cffi
                       'magicl.foreign-libraries::liblapack
                       parser)))

(defun parse-expokit-files (&optional (basedir *basedir*))
  "Right now, this only parses the dense matrix exponentiation routines, because the sparse ones call an external subroutine which is not handled by the parser."
  (let ((files (append
                (directory (merge-pathnames "fortran/*padm.f" basedir))
                (directory (merge-pathnames "fortran/*chbv.f" basedir)))))
    (mapcar #'parse-fortran-file files)))

(defun generate-expokit-file ()
  (generate-file "expokit-cffi"
                 '#:magicl.expokit-cffi
                 'magicl.foreign-libraries::libexpokit
                 #'parse-expokit-files))

(defun generate-expokit-files (expokit-dir)
  (let ((*basedir* expokit-dir))
    (generate-expokit-file)))
