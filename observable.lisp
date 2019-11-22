#|
 This file is a part of Alloy
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.alloy)

;;; TODO: This is useful enough to be its own library, I think.
(defstruct (observer (:constructor make-observer (name function))
                     (:predicate NIL)
                     (:copier NIL))
  (name NIL :type T)
  (function NIL :type function))

(defclass observable ()
  ((observers :initform (make-hash-table :test 'equal) :accessor observers)))

(defgeneric observe (function observable observer &optional name))
(defgeneric remove-observers (function observable &optional name))
(defgeneric list-observers (function observable))
(defgeneric notify-observers (function observable &rest args))
(defgeneric make-observable (function lambda-list &optional class))

(defmacro define-observable (name lambda-list &rest options)
  `(progn
     (defgeneric ,name ,lambda-list ,@options)
     (eval-when (:compile-toplevel :load-toplevel :execute)
       (make-observable ',name ',lambda-list))))

(defun gather-declarations (body)
  (let* ((declarations (loop for item = (car body)
                             while (and (listp item) (eql 'declare (car item)))
                             collect (pop body)))
         (normalized (loop for declaration in declarations
                           append (loop for part in (rest declaration)
                                        collect `(declare ,part)))))
    (values normalized body)))

(defmacro on (function args &body body)
  (multiple-value-bind (declarations body) (gather-declarations body)
    (let* ((position (or (if (listp function)
                             (get (second function) 'observable-setf-position)
                             (get function 'observable-position))
                         (error "The function~%  ~s~%is not observable." function)))
           (observable (nth position args))
           (args (copy-list args))
           (name (second (second (find 'name declarations :key #'caadr))))
           (declarations (remove 'name declarations :key #'caadr)))
      (setf (nth position args) 'observable)
      `(observe ',function ,observable
                (lambda ,args
                  ,@declarations
                  (declare (ignorable observable))
                  ,@body)
                ',name))))

(defmethod make-observable (function lambda-list &optional (class 'observable))
  (let ((pos (or (position class lambda-list)
                 (error "Cannot make ~s observable: the LAMBDA-LIST~%  ~s~%does not contain ~s"
                        function lambda-list class)))
        (lambda-list (copy-list lambda-list))
        (argvars ()))
    ;; Gather argvars and restructure lambda-list
    (loop for cons on lambda-list
          do (case (car cons)
               ((&key &rest)
                (setf (car cons) '&rest)
                (setf (cdr cons) (list (gensym "REST"))))
               (&optional)
               (observable
                (setf (car cons) (list class class))
                (push class argvars))
               (T
                (when (listp (car cons))
                  (setf (car cons) (caar cons)))
                (push (car cons) argvars)))
          finally (setf argvars (nreverse argvars)))
    ;; Save the position of the observable
    (etypecase function
      (cons (setf (get (second function) 'observable-setf-position) pos))
      (symbol (setf (get function 'observable-position) pos)))
    ;; Generate method
    ;; KLUDGE: Using the proper ADD-METHOD route would require MOP.
    (eval
     `(defmethod ,function :after ,lambda-list
        (,(if (find '&optional lambda-list) 'apply 'funcall)
         #'notify-observers ',function ,class ,@argvars)))))

(defmethod observe (function (observable observable) observer-function &optional name)
  (let* ((name (or name observer-function))
         (observer (find name (gethash function (observers observable)) :key #'observer-name)))
    (if observer
        (setf (observer-function observer) observer-function)
        (push (make-observer name observer-function) (gethash function (observers observable))))
    name))

(defmethod remove-observers (function (observable observable) &optional name)
  (if name
      (setf (gethash function (observers observable))
            (remove name (gethash function (observers observable)) :key #'observer-name))
      (remhash function (observers observable))))

(defmethod list-observers (function (observable observable))
  (loop for observer in (gethash function (observers observable))
        collect (observer-name observer)))

(defmethod notify-observers (function (observable observable) &rest args)
  (loop for observer in (gethash function (observers observable))
        do (apply (observer-function observer) args)))

(defclass observable-object (observable)
  ())

(macrolet ((def ()
             `(defmethod (setf c2mop:slot-value-using-class) :after (value class (object observable-object) slot)
                (when (slot-boundp object 'observers)
                  (notify-observers (c2mop:slot-definition-name slot) object value object)))))
  (def))

(defclass observable-table (observable)
  ((storage :reader storage)))

(defmethod initialize-instance :after ((table observable-table) &rest initargs)
  (setf (slot-value table 'storage) (apply #'make-hash-table initargs)))

(defun make-observable-table (&rest hash-table-initargs)
  (apply #'make-instance 'observable-table hash-table-initargs))

(defun gettable (key table &optional default)
  (gethash key (storage table) default))

(defun (setf gettable) (value key table)
  (prog1 (setf (gethash key (storage table)) value)
    (notify-observers '(setf gettable) table value key table)))

(defun remtable (key table)
  (prog1 (remhash key (storage table))
    (notify-observers 'remtable table key table)))

(defun clrtable (table)
  (prog1 (clrhash (storage table))
    (notify-observers 'clrtable table table)))
