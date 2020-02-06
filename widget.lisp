#|
 This file is a part of Alloy
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.alloy)

(defun remove-slotspec (name slots)
  (remove name slots :key (lambda (s) (getf s :name))))

(defun find-slotspec (name slots)
  (find name slots :key (lambda (s) (getf s :name))))

(defclass widget-class (standard-class)
  ((direct-initializers :initform () :reader direct-initializers)
   (effective-initializers :initform () :accessor effective-initializers)
   (foreign-slots :initform () :reader foreign-slots)
   (direct-slots :initform () :reader direct-slots)))

(defgeneric add-slot (name class &key if-exists))
(defgeneric remove-slot (name class))
(defgeneric add-initializer (name class &key priority function if-exists))
(defgeneric remove-initializer (name class))

(defmethod c2mop:validate-superclass ((a widget-class) (b T)) NIL)
(defmethod c2mop:validate-superclass ((a widget-class) (b standard-class)) T)
(defmethod c2mop:validate-superclass ((a widget-class) (b widget-class)) T)
(defmethod c2mop:validate-superclass ((a standard-class) (b widget-class)) NIL)

(defclass direct-initializer-slot (c2mop:standard-direct-slot-definition)
  ((usage :initarg :usage :initform NIL :accessor usage)
   (initializer-lambda :initarg :initializer :accessor initializer-lambda)))

(defmethod initialize-instance :after ((slot direct-initializer-slot) &key representation)
  (when representation
    (destructuring-bind (type &rest initargs) representation
      (setf (usage slot) :representation)
      (setf (initializer-lambda slot) `(lambda (widget)
                                         (declare (ignorable widget))
                                         (list ',type ,@initargs))))))

(defmethod c2mop:direct-slot-definition-class ((class widget-class) &key initializer representation)
  (if (or representation initializer)
      (find-class 'direct-initializer-slot)
      (call-next-method)))

(defclass effective-initializer-slot (c2mop:standard-effective-slot-definition)
  ((initializer :accessor initializer)))

(defclass effective-object-slot (effective-initializer-slot)
  ())

(defclass effective-representation-slot (effective-initializer-slot)
  ())

(defmethod c2mop:compute-effective-slot-definition ((class widget-class) name slots)
  (let ((slot (find name slots :key #'c2mop:slot-definition-name))
        (effective (call-next-method)))
    (when (typep slot 'direct-initializer-slot)
      (let ((proper (allocate-instance (ecase (usage slot)
                                         (:representation (find-class 'effective-representation-slot))
                                         (:object (find-class 'effective-object-slot))
                                         ((NIL) (find-class 'effective-initializer-slot))))))
        ;; Copy shit over because the MOP is bad.
        (dolist (slot (c2mop:class-slots (find-class 'c2mop:standard-effective-slot-definition)))
          (let ((name (c2mop:slot-definition-name slot)))
            (setf (slot-value proper name) (slot-value effective name))))
        (setf effective proper)
        ;; Build initializer function
        (setf (initializer effective) (compile NIL (initializer-lambda slot)))))
    effective))

(defmethod c2mop:finalize-inheritance :after ((class widget-class))
  (dolist (super (c2mop:class-direct-superclasses class))
    (unless (c2mop:class-finalized-p super)
      (c2mop:finalize-inheritance super)))
  (setf (effective-initializers class) (compute-effective-initializers class)))

(defmethod initialize-instance :after ((class widget-class) &key direct-slots)
  (setf (slot-value class 'direct-slots) direct-slots))

(defmethod reinitialize-instance :after ((class widget-class) &key (direct-initializers NIL d-p))
  (when d-p
    (setf (slot-value class 'direct-initializers) (stable-sort direct-initializers #'> :key #'second)))
  (setf (effective-initializers class) (compute-effective-initializers class))
  (make-instances-obsolete class))

(defmethod reinitialize-instance :around ((class widget-class) &rest options &key (foreign-slots NIL f-p) (direct-slots NIL d-p))
  (when f-p
    (setf (slot-value class 'foreign-slots) foreign-slots))
  (if d-p
      (setf (slot-value class 'direct-slots) direct-slots)
      (setf direct-slots (direct-slots class)))
  (let ((direct-slots (copy-list direct-slots)))
    (loop for spec in (foreign-slots class)
          for name = (getf spec :name)
          do (when (find-slotspec name direct-slots)
               (setf direct-slots (remove-slotspec name direct-slots))))
    (apply #'call-next-method class :direct-slots (append direct-slots (foreign-slots class)) options)))

(defmethod add-slot ((spec cons) (class widget-class) &key (if-exists :error))
  (unless (c2mop:class-finalized-p class)
    (c2mop:finalize-inheritance class))
  (let ((name (getf spec :name))
        (slotspecs (copy-list (foreign-slots class))))
    ;; Check against "normal" slots since you most likely do not want to thrash those.
    (when (and (find name (c2mop:class-slots class) :key #'c2mop:slot-definition-name)
               (not (find-slotspec name slotspecs)))
      (ecase if-exists
        (:error
         (error "FIXME: slot named ~s already exists as an inherited or direct slot!" name))
        (:supersede)
        ((NIL) (return-from add-slot NIL))))
    ;; Add the slotspec at the same position if it exists already
    (loop for slotspec on slotspecs
          do (when (eql name (getf (car slotspec) :name))
               (setf (car slotspec) spec)
               (return))
          finally (setf slotspecs (nconc slotspecs (list spec))))
    (reinitialize-instance class :foreign-slots slotspecs)
    spec))

(defmethod add-slot (name (class symbol) &rest args)
  (apply #'add-slot name (find-class class) args))

(defmethod remove-slot ((name symbol) (class widget-class))
  (reinitialize-instance class :foreign-slots (remove name (foreign-slots class) :key (lambda (s) (getf s :name))))
  name)

(defmethod remove-slot (name (class symbol))
  (remove-slot name (find-class class)))

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

(defmethod add-initializer (name (class symbol) &rest args)
  (apply #'add-initializer name (find-class class) args))

(defmethod remove-initializer ((name symbol) (class widget-class))
  (reinitialize-instance class :direct-initializers (remove name (direct-initializers class) :key #'car :test #'equal))
  name)

(defmethod remove-initializer (name (class symbol))
  (remove-initializer name (find-class class)))

(defmethod compute-effective-initializers ((class widget-class))
  (let ((initializers ()))
    (loop for super in (reverse (c2mop:compute-class-precedence-list class))
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

(defclass widget (observable-object)
  ((representations :initform (make-hash-table :test 'eq) :reader representations))
  (:metaclass widget-class))

(defmethod update-slot-value :around (slot (widget widget))
  (with-simple-restart (abort "Ignore the slot.")
    (call-next-method)))

(defmethod update-slot-value ((slot c2mop:standard-effective-slot-definition) (widget widget)))

(defmethod update-slot-value ((slot effective-initializer-slot) (widget widget))
  (setf (slot-value widget (c2mop:slot-definition-name slot)) (funcall (initializer slot) widget)))

(defmethod update-slot-value ((slot effective-object-slot) (widget widget))
  ;; KLUDGE: Ideally we'd know exactly which elements to remove to retain errors on
  ;;         double-enter or stealing an enter from a different container.
  (handler-bind ((element-has-different-parent
                   (lambda (c) (invoke-restart 'reparent))))
    (destructuring-bind (type &rest initargs) (funcall (initializer slot) widget)
      (let* ((name (c2mop:slot-definition-name slot))
             (value (if (slot-boundp widget name) (slot-value widget name) NIL)))
        (cond ((null value)
               (setf (slot-value widget name) (apply #'make-instance type initargs)))
              ((eq type (type-of value))
               (apply #'reinitialize-instance value initargs))
              (T
               (apply #'change-class value type initargs)))))))

(defmethod update-slot-value ((slot effective-representation-slot) (widget widget))
  (destructuring-bind (type &rest initargs) (funcall (initializer slot) widget)
    (let* ((name (c2mop:slot-definition-name slot))
           (value (gethash name (representations widget))))
      (cond ((null value)
             (setf (gethash name (representations widget))
                   (apply #'make-instance type :data (make-instance 'slot-data :slot name :object widget) initargs)))
            ((eq type (type-of value))
             (apply #'reinitialize-instance value initargs))
            (T
             (apply #'change-class value type initargs))))))

(defmethod shared-initialize :after ((widget widget) slots &key)
  (dolist (slot (c2mop:class-slots (class-of widget)))
    (update-slot-value slot widget)))

(defmethod initialize-instance :after ((widget widget) &key)
  (loop for initializer in (effective-initializers (class-of widget))
        do (funcall (third initializer) widget)))

(defmethod update-instance-for-redefined-class :after ((widget widget) added discarded plist &key)
  (declare (ignore added plist))
  ;; Remove representations of discarded slots
  (loop for name in discarded
        for representation = (gethash name (representations widget))
        do (when representation
             (leave representation T)
             (remhash name (representations widget)))))

(defmethod representation ((name symbol) (widget widget))
  (or (gethash name (representations widget))
      (error "The slot~%  ~s~%does not have a representation in~%  ~s" name widget)))

(defmacro define-widget (name direct-superclasses direct-slots &rest options)
  (unless (assoc :metaclass options)
    (push '(:metaclass widget-class) options))
  `(defclass ,name (,@direct-superclasses widget)
     ,direct-slots
     ,@options))

(defmacro define-slot ((widget name &optional usage &rest slot-args) &body body)
  `(add-slot '(:name ,name :initargs () :readers () :writers ()
               :usage ,usage :initializer (lambda (,widget)
                                            (declare (ignorable ,widget))
                                            ,@body))
             ',widget ,@slot-args))

(defmacro with-representations ((instance class) &body body)
  (let ((instanceg (gensym "INSTANCE"))
        (representations (gensym "REPRESENTATIONS")))
    `(let* ((,instanceg ,instance)
            (,representations (representations ,instanceg)))
       (declare (ignorable ,representations))
       (symbol-macrolet ,(loop for slot in (c2mop:class-slots (find-class class))
                               for name = (c2mop:slot-definition-name slot)
                               when (typep slot 'effective-initializer-slot)
                               collect `(,name ,(etypecase slot
                                                  (effective-representation-slot
                                                   `(gethash ',name ,representations))
                                                  (T `(slot-value ,instanceg ',name)))))
         ,@body))))

(defmacro define-subobject ((widget name &optional (priority 0) &rest slot-args) constructor &body body)
  (let ((thunk (gensym "THUNK")))
    `(flet ((,thunk (,widget)
              (let ((,name (slot-value ,widget ',name)))
                (declare (ignorable ,name))
                ,@body)))
       (declare (ignorable #',thunk))
       (define-slot (,widget ,name :object ,@slot-args)
         (list ,@constructor))
       ,(if body
            `(add-initializer ',name ',widget :priority ,priority :function #',thunk :if-exists :supersede)
            `(remove-initializer ',name ',widget)))))

(defmacro define-subcomponent ((widget name &optional (priority 0) &rest slot-args) (place class &rest initargs) &body body)
  `(define-subobject (,widget ,name ,priority ,@slot-args) (',class ,@initargs :data ,(expand-place-data place))
     ,@body))

(defmacro define-subbutton ((widget name &optional fun &rest slot-args) (&optional label (class 'button) &rest initargs) &body body)
  (let ((fun (or fun (intern (format NIL "~a-~a-~a" (symbol-name widget) (symbol-name name) (symbol-name 'activate)))))
        (label (or label (string-capitalize name))))
    `(progn
       (define-subcomponent (,widget ,name 0 ,@slot-args) (,label ,class ,@initargs))
       (declaim (observation ,fun (,widget ,name) activate))
       (defun ,fun (,widget ,name)
         (declare (ignorable ,widget ,name))
         ,@body))))

(defmacro define-subcontainer ((widget name &rest slot-args) (class &rest initargs) &body contents)
  `(define-slot (,widget ,name :object ,@slot-args)
     (with-representations (,widget ,widget)
       (list ',class ,@initargs :elements (list ,@contents)))))

(defun remove-subobject (widget name)
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
        (flet ((setup (instance)
                 (observe observed (if (listp widget)
                                       (slot-value instance (second widget))
                                       instance)
                          (lambda (&rest args)
                            (apply function instance args))
                          function)))
          (add-initializer function (if (listp widget) (first widget) widget)
                           :function #'setup :priority -100 :if-exists :supersede)))
      (cl:proclaim form)))

#+(or)
(progn
  (trivial-indent:define-indentation define-slot (4 4 &body))
  (trivial-indent:define-indentation define-subui (4 4 &body))
  (trivial-indent:define-indentation define-subcomponent (4 4 &body)))
