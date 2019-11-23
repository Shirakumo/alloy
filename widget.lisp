#|
 This file is a part of Alloy
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.alloy)

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

(defclass direct-representation-slot (c2mop:standard-direct-slot-definition)
  ((representation-form :initarg :representation :accessor representation-form)))

(defmethod c2mop:direct-slot-definition-class ((class widget-class) &key representation)
  (if representation
      (find-class 'direct-representation-slot)
      (call-next-method)))

(defclass effective-representation-slot (c2mop:standard-effective-slot-definition)
  ((initializer :accessor initializer)))

(defmethod c2mop:compute-effective-slot-definition ((class widget-class) name slots)
  (let ((slot (find name slots :key #'c2mop:slot-definition-name))
        (effective (call-next-method)))
    (when (typep slot 'direct-representation-slot)
      (let ((proper (allocate-instance (find-class 'effective-representation-slot))))
        ;; Copy shit over because the MOP is bad.
        (dolist (slot (c2mop:class-slots (find-class 'c2mop:standard-effective-slot-definition)))
          (let ((name (c2mop:slot-definition-name slot)))
            (setf (slot-value proper name) (slot-value effective name))))
        (setf effective proper)
        ;; Build initializer function
        (destructuring-bind (type &rest args) (representation-form slot)
          (setf (initializer effective) (compile NIL `(lambda () (list ',type ,@args)))))))
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
  (loop for name in (foreign-slots class)
        do (unless (find name direct-slots :key (lambda (s) (getf s :name)))
             (push (list :name name :readers NIL :writers NIL :initargs NIL) direct-slots)))
  (apply #'call-next-method class :direct-slots direct-slots options))

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

(defmethod add-slot (name (class symbol) &rest args)
  (apply #'add-slot name (find-class class) args))

(defmethod remove-slot ((name symbol) (class widget-class))
  (reinitialize-instance class :foreign-slots (remove name (foreign-slots class)))
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

(defmethod initialize-instance :after ((widget widget) &key)
  (loop for initializer in (effective-initializers (class-of widget))
        do (funcall (third initializer) widget)))

(defmethod shared-initialize :after ((widget widget) slots &key)
  ;; Update or add new slots
  (loop with representations = (representations widget)
        for slot in (c2mop:class-slots (class-of widget))
        for name = (c2mop:slot-definition-name slot)
        when (typep slot 'effective-representation-slot)
        do (let ((representation (gethash name representations))
                 (new-data (funcall (initializer slot))))
             (cond ((null representation)
                    (setf (gethash name representations)
                          (apply #'make-instance (first new-data)
                                 :data (make-instance 'slot-data :slot name :object widget)
                                 (rest new-data))))
                   ((eq (first new-data) (type-of representation))
                    (apply #'reinitialize-instance representation (rest new-data)))
                   (T
                    (apply #'change-class representation new-data))))))

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

(defmacro define-slot ((widget name &optional (priority 0)) constructor &body body)
  (let ((rep (gensym "REPRESENTATIONS")))
    `(flet ((,name (,widget)
              (let ((,rep (representations ,widget)))
                (flet ((rep (name)
                         (or (gethash name ,rep)
                             (error "a"))))
                  (declare (inline rep) (ignorable #'rep))
                  (let ((,name (setf (slot-value ,widget ',name) ,constructor)))
                    (declare (ignorable ,name))
                    ,@body)))))
       (add-slot ',name ',widget)
       (add-initializer ',name ',widget :function #',name :priority ,priority :if-exists :supersede))))

(defmacro with-representations ((instance class) &body body)
  (let ((instanceg (gensym "INSTANCE")))
    `(let ((,instanceg ,instance))
       (symbol-macrolet ,(loop for slot in (c2mop:class-direct-slots (find-class class))
                               for name = (c2mop:slot-definition-name slot)
                               collect `(,name (representation ',name ,instanceg)))
         ,@body))))

(defmacro define-subui ((widget name &optional (priority 0)) type &body body)
  `(define-slot (,widget ,name ,priority)
       (with-representations (,widget ,widget)
         (build-ui (,type ,@body)))))

(defmacro define-subcomponent ((widget name &optional (priority 0)) (place type &rest initargs) &body body)
  `(define-slot (,widget ,name ,priority) (represent ,place ,type ,@initargs)
     ,@body))

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
        (flet ((setup (instance)
                 (when (listp widget)
                   (setf instance (slot-value instance (second widget))))
                 (observe observed instance
                          (lambda (&rest args)
                            (apply function instance args))
                          function)))
          (add-initializer function widget :function #'setup :priority -100 :if-exists :supersede)))
      (cl:proclaim form)))

#+(or)
(progn
  (trivial-indent:define-indentation define-slot (4 4 &body))
  (trivial-indent:define-indentation define-subui (4 4 &body))
  (trivial-indent:define-indentation define-subcomponent (4 4 &body)))
