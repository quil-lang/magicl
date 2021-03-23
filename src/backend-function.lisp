;;;; backend-function.lisp

(in-package #:magicl.backends)

;;;; This file implements functions that may have different
;;;; "backends", specifically to support different kinds of vector and
;;;; matrix arithmetic routines. For instance, we might want both Lisp
;;;; and C implementations of the same function, and we might want the
;;;; user to be able to select such functions at run-time.
;;;;
;;;; Terminology:
;;;;
;;;;     BACKEND: A name of a backend, usually a keyword. Make a new
;;;;     one with DEFINE-BACKEND.
;;;;
;;;;     BACKEND FUNCTION: A function that "wraps"
;;;;     implementations. This is sort of like a Common Lisp generic
;;;;     function. Make a new one with DEFINE-BACKEND-FUNCTION.
;;;;
;;;;     BACKEND IMPLEMENTATION: A function that actually contains the
;;;;     code for a specific function's backend. This is sort of like
;;;;     a Common Lisp method. Make a new one with
;;;;     DEFINE-BACKEND-IMPLEMENTATION.

;;; first some utilities

(deftype function-designator ()
  '(or symbol function))

(defun normalize-optional/keyword-argument (arg)
  (destructuring-bind (arg init supplied) (alexandria:ensure-list arg)
    (list arg init (if (null supplied)
                       (gensym "SUPPLIEDP")
                       supplied))))

(defun normalize-lambda-list (lambda-list)
  (multiple-value-bind (required-args
                        optional-args                 ; List of (NAME INIT SUPPLIEDP)
                        rest-arg
                        kw-args                  ; List of ((KW NAME) INIT SUPPLIEDP)
                        allow-other-keys
                        aux-args)
      (alexandria:parse-ordinary-lambda-list lambda-list)
    (let ((normalized required-args))
      (when rest-arg
        (setf normalized (append normalized '(&rest) (list rest-arg))))
      (when optional-args
        (setf normalized
              (append normalized '(&optional)
                      (mapcar #'normalize-optional/keyword-argument optional-args))))
      (when kw-args
        (setf normalized
              (append normalized '(&key)
                      (mapcar #'normalize-optional/keyword-argument kw-args))))
      (when allow-other-keys
        (setf normalized (append normalized '(&allow-other-keys))))
      (when aux-args
        (setf normalized (append normalized '(&aux) aux-args)))
      normalized)))

;;; now the meat

(define-condition no-applicable-implementation (simple-error)
  ()
  (:documentation "The error to signal when there's no applicable implementation, whether from a backend function or a CLOS generic function."))

(defun no-applicable-implementation (name)
  "Call this function to signal an error indicating the caller (or any callers above it) are not applicable to the current backend function being invoked."
  (error 'no-applicable-implementation
         :format-control (format nil "~S is not implemented in any of ~
                                      the current active backends: ~
                                      ~~{~~A~~^, ~~}."
                                 name)
         :format-arguments (list (active-backends))))

(defmacro define-compatible-no-applicable-method-behavior (&rest generic-function-names)
  "Ensure the (unquoted symbol) generic function names GENERIC-FUNCTION-NAMES will behave correctly when used as implementations for backend functions.

The specific behavior that is made \"compatible\" is ensuring that if generic function dispatch reaches a call to NO-APPLIABLE-METHOD, then an appropriate condition will be signaled to instruct the backend function to continue searching.

Without using this, a backend function may error if no method is found."
  (assert (every #'symbolp generic-function-names))
  `(progn
     ,@(loop
         :for name :in generic-function-names
         :collect
         `(defmethod cl:no-applicable-method ((gf (eql ',name)) &rest args)
            (declare (ignore args))
            (no-applicable-implementation ',name)))))

;;; Backend Names

(defvar *known-backends* '()
  "A list of known backends.")

(defun backend-name-p (x)
  (and (symbolp x)
       (find x *known-backends*)
       t))

(deftype backend-name ()
  '(satisfies backend-name-p))

(defvar *backend* '()
  "A list in priority order the backends that MAGICL should use for functionality.

It is preferable to use WITH-BACKENDS instead of this.")

(defun active-backends ()
  "Return a list of the active backends in priority order. Useful for debugging."
  *backend*)

(defmacro define-backend (name &key documentation
                                    (default nil))
  "Define the existence of a backend named NAME. If DEFAULT is T, then add it to the list of default backends."
  (check-type name symbol)
  (check-type documentation (or null string))
  (check-type default boolean)
  `(progn
     (pushnew ',name *known-backends*)
     (when ',default
       (pushnew ',name *backend*))))

(define-backend :lisp
  :documentation "The pure Lisp backend."
  :default t)

(defmacro with-backends ((&rest backends) &body body)
  "Execute BODY with the (static) list of BACKENDS, written in the priority they should be used.

If you need fine-grained control over the backends selected, you may instead directly bind *BACKEND*, but it is not recommended."
  (dolist (backend backends)
    (unless (typep backend 'backend-name)
      (error "Not a backend name: ~A" backend)))
  `(let ((*backend* ',backends))
     ,@body))


;;; Storage of Backend Implementations
;;;
;;; We store backend implementations in the symbol plist. The
;;; implementations themselves form a plist with backends as keys and
;;; the implementation functions as values.

(defconstant +backends-key+ 'backend-implementations
  "The key to the symbol plist to get the backend implementations.")

(defun backend-function-p (x)
  "Does X name a backend function?"
  (and (symbolp x)
       (fboundp x)
       (not (eq '#1=#:not-found
                (getf (symbol-plist x) +backends-key+ '#1#)))))

(defun initialize-function-for-implementations (x)
  (check-type x symbol)
  (unless (backend-function-p x)
    (setf (getf (symbol-plist x) +backends-key+) nil)))

(defun %backend-implementation (sym backend)
  (getf (getf (symbol-plist sym) +backends-key+) backend))

(defun (setf %backend-implementation) (new-value sym backend)
  (check-type new-value function-designator)
  (check-type backend backend-name)
  (setf (getf (getf (symbol-plist sym) +backends-key+) backend) new-value))

(defun backend-implementation (sym backend)
  "Return the backend implementation for the function named SYM and backend named BACKEND."
  (%backend-implementation sym backend))

(define-compiler-macro backend-implementation (&whole whole sym backend &environment env)
  (cond
    ((and (constantp sym env)     (typep sym '(cons (member quote)
                                               (cons symbol null)))
          (constantp backend env) (typep backend '(or
                                                   backend-name
                                                   (cons (member quote)
                                                    (cons backend-name null)))))
     (let ((sym (second sym))           ; de-QUOTE
           (backend (if (typep backend 'backend-name)
                        backend
                        (second backend)))) ; de-QUOTE
       (%backend-implementation sym backend)))
    (t
     whole)))


;;; Definition of Backend Functions and Implementations

(defmacro define-backend-function (name lambda-list &optional doc)
  "Define a backend function named NAME with lambda list LAMBDA-LIST.

Backend implementations are defined with DEFINE-BACKEND-IMPLEMENTATION."
  (check-type name symbol)
  (check-type doc (or null string))
  (unless (eq (symbol-package name) (find-package "MAGICL"))
    (warn "Defining a backend function ~S in ~
           a package that's not MAGICL. Are you sure?"
          name))
  (setf lambda-list (normalize-lambda-list lambda-list))
  (let ((backend             (gensym "PROFILE"))
        (func                (gensym "FUNC")))
    `(progn
       (initialize-function-for-implementations ',name)
       (defun ,name ,lambda-list
         ,@(and doc (list doc))
         (dolist (,backend *backend*)
           (handler-case
               (let ((,func (backend-implementation ',name ,backend)))
                 (unless (null ,func)
                   (return-from ,name
                     ,(interface:calling-form func lambda-list))))
             (no-applicable-implementation (c)
               (declare (ignore c))
               nil)))
         (no-applicable-implementation ',name)))))

(defmacro define-backend-implementation (name backend funcallable-expression)
  "Define the implementation of the backend function named NAME, for the backend BACKEND, as the function FUNCALLABLE-EXPRESSION (evaluated).

NOTE: If your implementation is a generic function, then the generic function's behavior with NO-APPLICABLE-METHOD should be made compatible with the backends framework. This can be done with the macro DEFINE-COMPATIBLE-NO-APPLICABLE-METHOD-BEHAVIOR."
  (check-type name symbol)
  (check-type backend backend-name)
  `(progn
     (setf (%backend-implementation ',name ',backend) ,funcallable-expression)
     ',name))
