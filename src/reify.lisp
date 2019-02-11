;;;; reify.lisp
;;;;
;;;; Author: Robert Smith

(defpackage #:reify
  (:use #:common-lisp)
  (:export #:set-reification-procedures
           #:clear-reification-procedures))

(in-package #:reify)

#-sbcl
(progn
  (defun set-reification-procedures ()
    nil)
  
  (defun clear-reification-procedures ()
    nil)

)                                       ; Toplevel PROGN

#+sbcl
(progn
  (defun map-objects (f)
    (flet ((map-fn (obj type-code size)
             (declare (ignore type-code size))
             (funcall f obj)))
      (declare (dynamic-extent #'map-fn))
      (sb-vm::map-allocated-objects #'map-fn ':all)
      nil))

  (defvar *reification-procedures* (make-hash-table :test 'eq))

  (defun reify-exit-hook ()
    (loop :with f := (lambda (obj) (declare (ignore obj)) nil)
          :for type :being :the :hash-keys :of *reification-procedures*
            :using (hash-value exit/init)
          :for exit := (car exit/init)
          :do (setf f (let ((f f)
                            (type type)
                            (exit exit))
                        (lambda (obj)
                          (if (and (typep obj type)
                                   (not (null exit)))
                              (funcall exit obj)
                              (funcall f obj)))))
          :finally (map-objects f)))

  (defun reify-init-hook ()
    (loop :with f := (lambda (obj) (declare (ignore obj)) nil)
          :for type :being :the :hash-keys :of *reification-procedures*
            :using (hash-value exit/init)
          :for init := (cdr exit/init)
          :do (setf f (let ((f f)
                            (type type)
                            (init init))
                        (lambda (obj)
                          (if (and (typep obj type)
                                   (not (null init)))
                              (funcall init obj)
                              (funcall f obj)))))
          :finally (map-objects f)))

  (defun set-reification-procedures (type &key (exit nil exit-provided-p)
                                               (init nil init-provided-p))
    "For objects of type TYPE (a symbol), associate the functions EXIT and INIT for reification purposes. EXIT should be a unary function designator which takes an object of type TYPE and prepares it for reification. INIT should be a unary function designator which takes an object of type TYPE and reifies it accordingly.

If either :EXIT or :INIT are provided NIL, the respective procedure will be deleted."
    (check-type type symbol)
    (check-type exit (or null symbol function))
    (check-type init (or null symbol function))
    
    (let ((current (gethash type *reification-procedures*)))
      ;; Create a cons.
      (unless (consp current)
        (setf current (cons nil nil))
        (setf (gethash type *reification-procedures*) current))
      
      (when exit-provided-p
        (rplaca current exit))
      
      (when init-provided-p
        (rplacd current init))
      nil))

  (defun clear-reification-procedures (&optional (type nil type-provided-p))
    "Clear all reification procedures for all types, unless TYPE is a symbol, in which case only the reification procedures for TYPE are removed."
    (check-type type (or null symbol))
    (if type-provided-p
        (remhash type *reification-procedures*)
        (setf *reification-procedures* (make-hash-table :test 'eq)))
    nil)


  ;;; Tell SBCL about this.
  (eval-when (:compile-toplevel :load-toplevel :execute)
    (pushnew 'reify-exit-hook sb-ext:*save-hooks*)
    (pushnew 'reify-init-hook sb-ext:*init-hooks*))
 
)                                       ; Toplevel PROGN
