;; Copyright rif 2006.
;; Modified BSD License (see LICENSE file in this directory).

;; minimal changes, Copyright 2008, AJ Rossini <blindglobe@gmail.com>
;; Similar license.

(defpackage :org.middleangle.generate-blapack-interface
  (:nicknames :generate-blapack-interface)
  (:use :common-lisp
        :foreign-numeric-vector
        :blapack-cffi-types)
  (:export :generate-blapack-files))

(in-package :org.middleangle.generate-blapack-interface)

(declaim (optimize (safety 3) (debug 3) (speed 1)))

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
  '((("character") :string)
    (("character*") :string)
    (("character*1") :string)
    (("character*6") :string)
    (("integer") int32)                   ; Not clear this is right on 64 bit machines?
    (("real") float)
    (("double" "precision") double)
    (("complex") complex-float)           ; :complex-float and :complex-double
    (("double" "complex") complex-double) ; are from foreign-numeric-vector
    (("complex*16") complex-double)
    (("logical") :logical)
    (("none") :void)))


(defparameter *ctype-to-fortrantype*
  '((:string :string)
    (int32 fortran-int)
    (float fortran-float)
    (double fortran-double)
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
    (mapcar #'string-downcase (tokenize line))))

(defun parse-signature ()
  "Parses the name of the function and the names of all the arguments.
Returns three values: the name, the list of arguments, and all
remaining lines."
  (let ((line (extract-continued-line))
	(return-type (list "none"))
	name vars)
    (if (string= (car line) "subroutine")
	(setf name (second line)
	      vars (nthcdr 3 line))
	(let ((fpos (position "function" line :test #'string=)))
	  (if fpos
	      (setf name (nth (1+ fpos) line)
		    return-type (subseq line 0 fpos)
		    vars (nthcdr (+ 3 fpos) line))
	      (error "Can't parse routine: ~A | ~A" (car line) line))))
    (values name (butlast vars) return-type)))

(defun extract-type (line)
  (let ((type 
	 (find-if (lambda (e) 
		    (every #'string= e (subseq line 0 (length e))))
		  *types*)))
    (unless type
      (error "Can't find type: ~A" line))
    (values type (subseq line (length type)))))
		  
(defun fill-in-type (names type vars array-maps)
  (mapcar (lambda (v)
	    (let ((pair (assoc v names :test #'string=)))
	      (when pair
		(setf (cdr pair) 
		      (append type
			      (cdr (assoc v array-maps :test #'string=)))))))
	  vars))


(defun deparenthesize (line)
  "Get rid of stupid parentheses around array arguments."
  (let ((keepers nil)
	(array-maps nil)
	(in-array nil)
	(prev nil))
    (loop for i in line do
	  (cond ((string= i "(" )
		 (push prev in-array))
		((string= i ")" )
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
		(multiple-value-bind (type vars)
		    (extract-type line)
		  (fill-in-type names type vars array-maps))))))
    names))

(defun parse-fortran-file (fortran-file)
  (setf *lines* (read-lines fortran-file))
    (multiple-value-bind (name vars return-type)
	(parse-signature)
      (values name (parse-argument-types vars) return-type)))

;; Edit these if you want to change the input/output locations!
(defparameter *basedir* #p"/home/rif/software/LAPACK/")
(defparameter *outdir*
  (make-pathname :directory
                 (pathname-directory
                  (truename (asdf:system-definition-pathname
                             (asdf:find-system
                              :org.middleangle.cl-blapack-gen))))))

(defmacro fortran-dir-parsing-fn (fn-name fortran-files-wildcard-string)
  `(defun ,fn-name  (&optional (basedir *basedir*))
     (let ((files
	    (directory
	     (pathname 
	      (concatenate 'string (namestring basedir)
			   ,fortran-files-wildcard-string)))))
       (mapcar (lambda (f)
		 (multiple-value-bind (name vars ret)
		     (parse-fortran-file f)
		   (list name vars ret)))
	       files))))

;; above macro WHEN PROPERLY written (still need to verify the above)
;; should simplify the following two functions into something like:
;;
;; (defvar *basedir* "testme/")
;; (fortran-dir-parsing-fn my-parse-blas-files "BLAS/SRC/*.f") ; slime: C-c M-m
;; (fortran-dir-parsing-fn my-parse-lapack-files "SRC/*.f")    ; slime: C-c M-m
;; 
;; The reason is that we'd like to add in other Fortran libraries if
;; possible, and factor out the auto-gen bindings into a
;; cffi-fortran-grovel package.

(defun parse-blas-files (&optional (basedir *basedir*))
  (let ((files
	 (directory
	  (pathname 
	   (concatenate 'string
			(namestring basedir) "BLAS/SRC/*.f")))))
    (mapcar (lambda (f)
	      (multiple-value-bind (name vars ret)
		  (parse-fortran-file f)
		(list name vars ret)))
	    files)))

(defun parse-lapack-files (&optional (basedir *basedir*)) 
  (let ((files
	 (directory
	  (pathname 
	   (concatenate 'string (namestring basedir) "SRC/*.f")))))
    (mapcar (lambda (f)
	      (multiple-value-bind (name vars ret)
		  (parse-fortran-file f)
		(list name vars ret)))
	    files)))


(defun lookup-type (type-string-list &optional (return-type nil))
  (let ((base-type
	 (cadr 
	  (find-if
	   (lambda (type-map)
	     (and (>= (length type-string-list) (length type-map))
		  (every #'string= 
			 type-map
			 (subseq type-string-list 0 (length type-map)))))
	   *typemap*
	   :key #'car))))
    (if (member "*" type-string-list :test #'string=)
	(cffi-type-to-fnv-type base-type)
	(if return-type base-type
	    (cffi-type-to-fortran-type base-type)))))


(defun fortran-transform (name)
  "Turns a fortran library function into a C library function.  May
need to be customized."
  (concatenate 'string name "_"))

(defun safe-string (string)
  "Use of T as an argument is a proble -- in particular, there are
issues with CFFI when using T as a function arg vs. the CL value."
  (if (string= string "T") "TT" string))

(defun generate-cffi-interface (parsed-representation)
  (destructuring-bind (name vars return-type) parsed-representation
    `(cffi::defcfun 
      (,(fortran-transform name) ,(intern 
	       (concatenate 'string "%" (string-upcase name))))
      ,(lookup-type return-type t)
      ,@(mapcar (lambda (v)
		  (list (intern (safe-string (string-upcase (car v))))
			(lookup-type (cdr v))))
		vars))))

(defun terpri2 (str)
  "Square the terpri!"
  (terpri str) (terpri str))

(defun generate-bindings-file (filename package-name nickname bindings
			       &optional (outdir *outdir*))
  "This does the bulk of the work in getting things automagically
done, and is used by generate-blas-bindings etc to automagically do
the CFFI binding file."
  (with-open-file (f (make-pathname :name filename 
				    :type "lisp" 
				    :defaults outdir)
		     :direction :output
		     :if-exists :supersede)
    (write 
     `(defpackage ,package-name
       (:nicknames ,nickname)
       (:use :common-lisp :cffi :foreign-numeric-vector
             :blapack-ffi-types))
     :stream f)
    (terpri2 f)
    (write 
     `(in-package ,package-name)
     :stream f)
    (terpri2 f)
    (map nil
	 (lambda (cffi-def)
	   (write cffi-def :stream f)
	   (terpri f)
	   (write `(export ',(cadadr cffi-def) ,nickname) :stream f)
	   (terpri2 f))
	 bindings)))

(defun generate-blas-file ()
  (generate-bindings-file 
   "blas-cffi"
   :org.middleangle.cl-blapack.blas-cffi
   :blas-cffi
   (mapcar #'generate-cffi-interface (parse-blas-files))))

(defun generate-lapack-file ()
  (generate-bindings-file 
   "lapack-cffi"
   :org.middleangle.cl-blapack.lapack-cffi
   :lapack-cffi
   (mapcar #'generate-cffi-interface (parse-lapack-files))))

(defun generate-blapack-files (&optional (basedir *basedir*))
  (let ((*basedir* basedir))
    (generate-blas-file)
    (generate-lapack-file)))
