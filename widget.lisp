#|
 This file is a part of Alloy
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.alloy)

(defclass widget-class (standard-class)
  ((direct-initializers :initform () :reader direct-initializers)
   (effective-initializers :initform () :accessor effective-initializers)
   (foreign-slots :initform () :reader foreign-slots)))

(defmethod c2mop:validate-superclass ((a widget-class) (b T)) NIL)
(defmethod c2mop:validate-superclass ((a widget-class) (b standard-class)) T)
(defmethod c2mop:validate-superclass ((a widget-class) (b widget-class)) T)
(defmethod c2mop:validate-superclass ((a standard-class) (b widget-class)) NIL)

(defmethod c2mop:finalize-inheritance :after ((class widget-class))
  (dolist (super (c2mop:class-direct-superclasses class))
    (unless (c2mop:class-finalized-p super)
      (c2mop:finalize-inheritance super)))
  (setf (effective-initializers class) (compute-effective-initializers class)))

(defmethod reinitialize-instance :after ((class widget-class) &key (direct-initializers NIL d-p))
  (when d-p
    (setf (slot-value class 'direct-initializers) (stable-sort direct-initializers #'> :key #'second)))
  (setf (effective-initializers class) (compute-effective-initializers class))
  (make-instances-obsolete class))

(defmethod reinitialize-instance :around ((class widget-class) &rest options &key (foreign-slots NIL f-p) direct-slots)
  (when f-p
    (setf (slot-value class 'foreign-slots) foreign-slots))
  (loop for name in (foreign-slots class)
        do (unless (find name direct-slots :key (lambda (s) (getf s :name)))
             (push (list :name name :readers NIL :writers NIL :initargs NIL) direct-slots)))
  (call-next-method class :direct-slots direct-slots options))

(defmethod add-slot ((name symbol) (class widget-class) &key (if-exists :error))
  ;; Check against "normal" slots since you most likely do not want to thrash those.
  (unless (c2mop:class-finalized-p class)
    (c2mop:finalize-inheritance class))
  (when (and (not (find name (foreign-slots class)))
             (find name (c2mop:class-slots class) :key #'c2mop:slot-definition-name))
    (ecase if-exists
      (:error
       (error "FIXME: slot named ~s already exists as an inherited or direct slot!" name))
      (:supersede)
      ((NIL) (return-from add-slot NIL))))
  (reinitialize-instance class :foreign-slots (list* name (foreign-slots class)))
  name)

(defmethod remove-slot ((name symbol) (class widget-class))
  (reinitialize-instance class :foreign-slots (remove name (foreign-slots class)))
  name)

(defmethod add-initializer ((name symbol) (class widget-class) &key priority (function name) (if-exists :error))
  (let* ((init (direct-initializers class))
         (existing (assoc name init))
         (priority (or priority 0)))
    (check-type priority real)
    (check-type function (or function symbol))
    (if existing
        (ecase if-exists
          (:error
           (error "FIXME: initializer named ~s already exists." name))
          (:supersede
           (setf (cdr existing) (list priority function)))
          (:move-to-front
           (setf init (list* (list name priority function) (remove name init :key #'car :test #'equal))))
          ((NIL)))
        (push (list name priority function) init))
    (reinitialize-instance class :direct-initializers init)
    (unless (and existing (eq if-exists NIL))
      name)))

(defmethod remove-initializer ((name symbol) (class widget-class))
  (reinitialize-instance class :direct-initializers (remove name (direct-initializers class) :key #'car :test #'equal))
  name)

(defmethod compute-effective-initializers ((class widget-class))
  (let ((initializers ()))
    (loop for super in (reverse (c2mop:class-precedence-list class))
          do (when (typep super 'widget-class)
               (setf initializers (merge-initializers initializers (direct-initializers super)))))
    (merge-initializers initializers (direct-initializers class))))

(defun merge-initializers (initializers override)
  (let ((initializers (copy-alist initializers))
        (override (copy-alist override)))
    ;; replace all initializers that have same name & priority
    ;; remove ones that do not and push the new one to front instead.
    (loop for (name prio . data) in override
          for orig = (assoc name initializers)
          do (cond ((null orig)
                    (push (list* name prio data) initializers))
                   ((/= prio (second orig))
                    (setf initializers (list* (list* name prio data)
                                              (remove name data :key #'car :test #'equal))))
                   (T
                    (setf (cdr orig) (list* prio data)))))
    (stable-sort initializers #'> :key #'second)))

(defmethod run-initializers ((class widget-class) instance)
  (loop for initializer in (effective-initializers class)
        do (funcall (third initializer) instance)))

(defclass widget (observable)
  ()
  (:metaclass widget-class))

(defmethod shared-initialize :after ((widget widget) slots &key)
  ;; Run initializers here to ensure they will be run again when u-i-f-r-c runs.
  ;; This will also cause initilaizers to run again when the instance is reinitialized
  ;; on its own, but I do not expect that to be a big deal.
  (run-initializers (class-of widget) widget))

(defmacro define-subwidget ((widget name &optional priority) type &body options)
  `(flet ((,name (,widget)
            (let ((,name (setf (slot-value ,widget ',name) (make-instance ',type initargs))))
              ,@body)))
     (add-slot ',name ',widget)
     (add-initializer ',name ',widget :function #',name :priority ,priority :if-exists :supersede)))

(defun remove-subwidget (widget name)
  (remove-slot name widget)
  (remove-initializer name widget))

;;; CLTL2's define-declaration does not guarantee that the defined declaration
;;; will work with declaim/proclaim, so we're doing the only thing we can.
;;; Believe me, I sighed a lot.
(defmacro declaim (&rest declarations)
  `(progn ,@(loop for declaration in declarations
                  collect (if (eq (car declaration) 'observation)
                              `(proclaim ',declaration)
                              `(cl:declaim ,declaration)))))

(defun proclaim (form)
  (if (eq (car form) 'observation)
      (destructuring-bind (function widget observed) (rest form)
        ;; KLUDGE: This means a function can only observe one particular change
        ;;         per widget class.
        (flet ((setup (widget)
                 (observe observed widget
                          (lambda (&rest args)
                            (apply function widget args))
                          function)))
          (add-initializer function widget :function #'setup :priority -100 :if-exists :supersede)))
      (cl:proclaim form)))
