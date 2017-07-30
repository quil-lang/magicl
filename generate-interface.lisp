(defpackage #:magicl.generate-interface
  (:nicknames #:generate-interface)
  (:use :common-lisp
        :foreign-numeric-vector
        :fnv-utils
        :cffi-types)
  (:export #:generate-blapack-files))

(in-package #:magicl.generate-interface)

(declaim (optimize (safety 3) (debug 3) (speed 1)))

;; can we use FSBV?

(defun read-lines (file)
  "Returns a list of strings, one string per line of file."
  (with-open-file (f file :direction :input)
    (loop for line = (read-line f nil) when line collect line while line)))

(defun comment-line-p (line)
  (member (first line) (list "c" "*") :test #'string=))

(defun to-string (char-list)
" Original def:
   (let* ((n (length char-list))
 	 (s (make-string n)))
     (dotimes (i n)
       (setf (char s i) (elt char-list i)))
     s))  "
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
  '((:fortran-string               :pointer                   array)
    (:fortran-int                  cffi-fnv-int32             fnv-int32)
    (:fortran-single-float         cffi-fnv-float             fnv-float)
    (:fortran-double-float         cffi-fnv-double            fnv-double)
    (:fortran-complex-single-float cffi-fnv-complex-float     fnv-complex-float)
    (:fortran-complex-double-float cffi-fnv-complex-double    fnv-complex-double)
    (:fortran-logical              cffi-fnv-int32             fnv-int32)
    (:fortran-none                 :pointer                   array)))

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
  '((:string :string)
    (:int32 fortran-int)
    (:float fortran-float)
    (:double fortran-double)
    (complex-float fortran-complex-float)
    (complex-double fortran-complex-double)
    (:logical fortran-logical)))

(defun cffi-type-to-fortran-type (cffi-type)
  (cadr (assoc cffi-type *ctype-to-fortrantype*)))

(defun extract-continued-line ()
  "Fortran can't have long lines, and so will use a $ in column 7 to
indicate a continued line.  We'll pop off these $'s and put them
together into one line, and return that extended line and the remainder."
  (let ((line (pop *lines*)))
    (loop while (and (>= (length (first *lines*)) 6)
		     (char= (char (first *lines*) 5) #\$)) do
          (setf line (concatenate 'string line (subseq (pop *lines*) 6))))
    (tokenize line)))

(defun parse-signature ()
  "Parses the name of the function and the names of all the arguments.
Returns three values: the name, the list of arguments, and all
remaining lines."
  (let* ((line (extract-continued-line))
         (subroutine-line (cdr (member "subroutine" line :test #'string=)))
         (function-line (cdr (member "function" line :test #'string=)))
         (return-type (list "none"))
         signature name vars)
     (if subroutine-line 
        (setf signature subroutine-line)
        (if function-line 
            (setf signature function-line)))
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
      (setf type (list (concatenate 'string (first type) "*"))))
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
  (make-pathname :directory
                 (pathname-directory
                  (truename (asdf:system-definition-pathname
                             (asdf:find-system
                              :magicl-gen))))))

(defun parse-blas-files (&optional (basedir *basedir*))
  (let ((files
	 (directory
	  (pathname
	   (concatenate 'string
			(namestring basedir) "BLAS/SRC/*.f")))))
    (mapcar #'parse-fortran-file files)))

(defun parse-lapack-files (&optional (basedir *basedir*))
  (let ((files
	 (directory
	  (pathname
	   (concatenate 'string (namestring basedir) "SRC/*.f")))))
    (mapcar #'parse-fortran-file files)))

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
    `(cffi::defcfun
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

(defun generate-cffi-interface-alternate (ff)
  (labels ((sym (var-name)
             (intern (string-upcase var-name) *package*)))
    (let* ((name (fortran-function-name ff))
           (vars-types (fortran-function-arguments ff))
           (vars (mapcar (lambda (vt) (sym (first vt))) vars-types))
           (ref-vars (mapcar (lambda (v) (gentemp (concatenate 'string (symbol-name v) "-REF")
                                                  *package*))
                             vars))
           (normalized-types (mapcar #'second vars-types))
           (return-type (fortran-function-return-type ff))
           (raw-call-name (sym (concatenate 'string "%%" (string-upcase name))))
           (lisp-fun-name (sym (concatenate 'string "%" (string-upcase name)))))
      (list
       ;; CFFI form
       `(cffi:defcfun (,(fortran-mangle-name name) ,raw-call-name)
            ,(normalized-type-to-cffi-type return-type ':immediate)
          ,@(loop :for var :in vars
                  :for norm-type :in normalized-types
                  ;; TODO: add more type information
                  :collect (list (sym var) (if (eq norm-type ':fortran-string)
                                               ':string
                                               ':pointer))))

       ;; Lisp function form
       `(cl:defun ,lisp-fun-name ,(mapcar #'sym vars)
          ;; Type declaration expressions
          (cl:declare ,@(loop :for var :in vars
                              :for norm-type :in normalized-types
                              :collect `(cl:type ,(normalized-type-to-cffi-type
                                                   norm-type
                                                   ':lisp)
                                                 ,var )))
          ;; Foreign allocation
          (cffi:with-foreign-objects ,(loop :for ref-var :in ref-vars
                                            :for norm-type :in normalized-types
                                            :when (and (atom norm-type)
                                                       (not (eq ':fortran-string norm-type)))
                                              :collect `(,ref-var
                                                         ',(normalized-type-to-cffi-type norm-type ':immediate)))
            ;; Setters
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
            ;; Call
            (,raw-call-name
             ,@(loop :for var :in vars
                     :for ref-var :in ref-vars
                     :for norm-type :in normalized-types
                     :collect (cond
                                ((eq ':fortran-string norm-type) var)
                                ((atom norm-type) ref-var)
                                (t `(fnv:fnv-foreign-pointer ,var)))))))))))

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

      (prin1 `(in-package ,package-name) f)
      (terpri f)
      (terpri f)
      (dolist (form bindings)
        (prin1 form f)
        (terpri f)
        (when (eq 'cl:defun (car form))
          (prin1 `(export ',(cadr form) ',package-name) f)
          (terpri f))
        (terpri f)))))

(defun generate-blas-file ()
  (let* ((package-name '#:magicl.blas-cffi)
         (*package* (find-package package-name)))
    (generate-bindings-file
     "blas-cffi"
     package-name
     (mapcan #'generate-cffi-interface-alternate (parse-blas-files)))))

(defun generate-lapack-file ()
  (let* ((package-name '#:magicl.lapack-cffi)
         (*package* (find-package package-name)))
    (generate-bindings-file
     "lapack-cffi"
     '#:magicl.lapack-cffi
     (mapcan #'generate-cffi-interface-alternate (parse-lapack-files)))))

(defun generate-blapack-files (&optional (basedir *basedir*))
  (let ((*basedir* basedir))
    (generate-blas-file)
    (generate-lapack-file)))
