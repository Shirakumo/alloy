#|
 This file is a part of Alloy
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.alloy)

(defclass data (observable)
  ())

(defgeneric refresh (data))
(defgeneric expand-place-data (place))
(defgeneric expand-compound-place-data (place args))
(defgeneric access (data field))
(defgeneric (setf access) (value data field))

(defmethod expand-place-data ((place cons))
  (expand-compound-place-data (first place) (rest place)))

(defmacro place-data (place)
  (expand-place-data place))

(defmethod access ((object standard-object) field)
  (slot-value object field))

(defmethod (setf access) (value (object standard-object) field)
  (setf (slot-value object field) value))

(defclass value-data (data)
  ((value :initarg :value :accessor value)))

(defgeneric value (data))
(defgeneric (setf value) (new-value data))
(make-observable '(setf value) '(new-value observable))

(defmethod value ((string string)) string)

(defmethod access ((data value-data) (field (eql 'value)))
  (value data))

(defmethod (setf access) (value (data value-data) (field (eql 'value)))
  (setf (value data) value))

(defmethod refresh ((data value-data))
  (notify-observers 'value data (value data) data))

(defmethod expand-place-data (atom)
  `(make-instance 'value-data :value ,atom))

(defclass object-data (data)
  ((object :initarg :object :initform (arg! :object) :accessor object)))

(defmethod (setf object) :around (value (data object-data))
  (observe NIL (object data) data)
  (prog1 (call-next-method)
    (observe T (object data) data)))

(defmethod access ((data object-data) field)
  (access (object data) field))

(defmethod (setf access) (value (data object-data) field)
  (setf (access (object data) field) value))

(defclass delegate-data (object-data)
  ((observed :initarg :observed :initform () :accessor observed)))

(defmethod reinitialize-instance :before ((data delegate-data) &key)
  (observe NIL (object data) data))

(defmethod shared-initialize :after ((data delegate-data) slots &key)
  (observe T (object data) data))

(defmethod observe ((nothing (eql NIL)) object (data delegate-data) &optional (name data))
  (dolist (function (observed data))
    (remove-observers function object name)))

(defmethod observe ((all (eql T)) object (data delegate-data) &optional (name data))
  (dolist (function (observed data))
    (observe function object (lambda (&rest args) (apply #'notify-observers function data args)) name))
  (refresh data))

(defmethod observe :after (function (data delegate-data) observer &optional name)
  (declare (ignore name))
  (unless (find function (observed data))
    (push function (observed data))))

(defmethod (setf observed) :around (value (data delegate-data))
  (observe NIL (object data) data)
  (prog1 (call-next-method)
    (observe T (object data) data)))

(defmethod refresh ((data delegate-data))
  ;; FIXME: do this. somehow.
  )

(defclass remap-data (object-data)
  ((mapping :initform (make-hash-table :test 'eql) :accessor mapping)))

(defmethod shared-initialize :after ((data remap-data) slots &key (mapping NIL mapping-p))
  (when mapping-p (setf (mapping data) mapping)))

(defmethod (setf mapping) :around ((value hash-table) (data remap-data))
  (observe NIL (object data) data)
  (prog1 (call-next-method)
    (observe T (object data) data)))

(defmethod (setf mapping) ((value cons) (data remap-data))
  (let ((table (make-hash-table :test 'eql)))
    (loop for (k . v) in value
          do (setf (gethash k table) v))
    (setf (mapping data) table)))

(defmethod (setf mapping) ((value null) (data remap-data))
  (let ((table (make-hash-table :test 'eql)))
    (setf (mapping data) table)))

(defmethod observe ((nothing (eql NIL)) object (data remap-data) &optional (name data))
  (loop for function being the hash-keys of (observed data)
        do (remove-observers function object name)))

(defmethod observe ((all (eql T)) object (data remap-data) &optional (name data))
  (loop for function being the hash-keys of (observed data) using (hash-value mapped)
        do (observe function object (lambda (&rest args) (apply #'notify-observers mapped data args)) name))
  (refresh data))

(defmethod refresh ((data remap-data))
  ;; FIXME: do this. somehow.
  )

;;; General case.
(defclass place-data (value-data)
  ((getter :initarg :getter :initform (arg! :getter) :accessor getter)
   (setter :initarg :setter :initform (arg! :setter) :accessor setter)))

(defmethod value ((data place-data))
  (funcall (getter data)))

(defmethod (setf value) (new-value (data place-data))
  (funcall (setter data) new-value))

(defmethod expand-compound-place-data ((place symbol) args)
  (let ((value (gensym "VALUE")))
    (if (and (fboundp place)
             (fboundp `(setf ,place))
             (null (rest args)))
        `(make-instance 'accessor-data
                        :object ,(first args)
                        :accessor ',place)
        `(make-instance 'place-data
                        :getter (lambda () (,place ,@args))
                        :setter (lambda (,value) (setf (,place ,@args) ,value))))))

(defmethod expand-place-data ((place symbol))
  (let ((value (gensym "VALUE")))
    `(make-instance 'place-data
                    :getter (lambda () ,place)
                    :setter (lambda (,value) (setf ,place ,value)))))

(defclass accessor-data (value-data object-data)
  ((accessor :initarg :accessor :initform (arg! :accessor) :accessor accessor)))

(defmethod initialize-instance :after ((data accessor-data) &key)
  (when (typep (object data) 'observable)
    (observe (accessor data) (object data) (lambda (value object) (notify-observers 'value data value object)) data)))

(defmethod value ((data accessor-data))
  (funcall (accessor data) (object data)))

(defmethod (setf value) (new-value (data accessor-data))
  (funcall (fdefinition `(setf ,(accessor data))) new-value (object data)))

(defmethod (setf object) :around (value (data accessor-data))
  (remove-observers (accessor data) (object data) data)
  (call-next-method)
  (observe (accessor data) (object data) (lambda (value object) (notify-observers 'value data value object)) data)
  (refresh data))

(defmethod (setf accessor) :around (value (data accessor-data))
  (remove-observers (accessor data) (object data) data)
  (call-next-method)
  (observe (accessor data) (object data) (lambda (value object) (notify-observers 'value data value object)) data)
  (refresh data))

(defclass slot-data (value-data object-data)
  ((slot :initarg :slot :initform (arg! :slot) :accessor slot)))

(defmethod initialize-instance :after ((data slot-data) &key)
  (when (typep (object data) 'observable)
    (observe (slot data) (object data) (lambda (value object) (notify-observers 'value data value object)) data)))

(defmethod value ((data slot-data))
  (slot-value (object data) (slot data)))

(defmethod (setf value) (new-value (data slot-data))
  (setf (slot-value (object data) (slot data)) new-value))

(defmethod observe ((nothing (eql NIL)) object (data slot-data) &optional (name data))
  (remove-observers (slot data) object name))

(defmethod observe ((all (eql T)) object (data slot-data) &optional (name data))
  (observe (slot data) object (lambda (value object) (notify-observers 'value data value object)) name)
  (refresh data))

(defmethod (setf slot) :around (value (data slot-data))
  (observe NIL (object data) data)
  (prog1 (call-next-method)
    (observe T (object data) data)))

(defmethod expand-compound-place-data ((place (eql 'slot-value)) args)
  (destructuring-bind (object slot) args
    `(make-instance 'slot-data :object ,object :slot ,slot)))

(defclass aref-data (value-data)
  ((object :initarg :object :initform (arg! :object) :accessor object)
   (index :initarg :index :initform (arg! :index) :accessor index)))

(defmethod value ((data aref-data))
  (row-major-aref (object data) (index data)))

(defmethod (setf value) (new-value (data aref-data))
  (setf (row-major-aref (object data) (index data)) new-value))

(defmethod expand-compound-place-data ((place (eql 'aref)) args)
  (let ((object (gensym "OBJECT")))
    `(let ((,object ,(first args)))
       (make-instance 'aref-data :object ,object :index (array-row-major-index ,object ,@(rest args))))))

(defmethod (setf index) ((list list) (data aref-data))
  (setf (index data) (apply #'array-row-major-index (object data) list)))

(defmethod (setf index) :after (index (data aref-data))
  (refresh data))

;;; TODO: This is kinda... not too great.
(defclass computed-data (value-data)
  ((closure :initarg :closure :accessor closure)))

(defmethod initialize-instance :after ((data computed-data) &key observe)
  (flet ((update (&rest _)
           (declare (ignore _))
           (setf (value data) (apply (closure data)
                                     (loop for (function object) in observe
                                           collect (funcall function object))))))
    (loop for (function object) in observe
          do (observe function object #'update))
    (update)))

(defmethod expand-compound-place-data ((place (eql 'lambda)) args)
  (destructuring-bind (args &rest body) args
    `(make-instance 'computed-data
                    :closure (,place ,(mapcar #'first args) ,@body)
                    :observe (list ,@(loop for (function object) in (mapcar #'second args)
                                           collect `(list ',function ,object))))))

(defclass sequence-data (data)
  ((value :initarg :sequence :initform (arg! :sequence) :reader value)))

(defgeneric element (data index))
(defgeneric (setf element) (value data index))
(defgeneric count (data))
(defgeneric push-element (value data &optional index))
(defgeneric pop-element (data &optional index))
(make-observable '(setf element) '(value observable index))
(make-observable 'push-element '(value observable &optional index))
(make-observable 'pop-element '(observable &optional index))

;; Defaults for a generic version with support for extensible sequences without having to
;; explicitly depend on that protocol
(defmethod element ((data sequence-data) (index integer))
  (elt (value data) index))

(defmethod (setf element) (value (data sequence-data) (index integer))
  (setf (elt (value data) index) value))

(defmethod count ((data sequence-data))
  (length (value data)))

(defclass list-data (sequence-data)
  ((value :initarg :list :initform (arg! :list) :reader value)
   (count :reader count)))

(defmethod initialize-instance :after ((data list-data) &key list)
  (setf (slot-value data 'count) (length list)))

(defmethod shared-initialize :before ((data list-data) slots &key (list NIL list-p))
  (when list-p
    (check-type list list)))

(defmethod refresh ((data list-data))
  (setf (slot-value data 'count) (length (value data))))

(defmethod element ((data list-data) (index integer))
  (nth index (value data)))

(defmethod (setf element) (value (data list-data) (index integer))
  (setf (nth index (value data)) value))

(defmethod push-element (value (data list-data) &optional index)
  (if (and index (< 0 index))
      (let ((cons (nthcdr (1- index) (value data))))
        (setf (cdr cons) (list* value (cddr cons))))
      (push value (value data)))
  (incf (slot-value data 'count)))

(defmethod pop-element ((data list-data) &optional index)
  (decf (slot-value data 'count))
  (if (and index (< 0 index))
      (let ((cons (nthcdr (1- index) (value data))))
        (prog1 (cadr cons)
          (setf (cdr cons) (cddr cons))))
      (pop (value data))))

(defclass vector-data (sequence-data)
  ((value :initarg :vector :initform (arg! :vector) :reader value)))

(defmethod shared-initialize :before ((data vector-data) slots &key (vector NIL vector-p))
  (when vector-p
    (check-type vector vector)))

(defmethod element ((data vector-data) (index integer))
  (aref (value data) index))

(defmethod (setf element) (value (data vector-data) (index integer))
  (setf (aref (value data) index) value))

(defmethod push-element (value (data vector-data) &optional index)
  (if index
      (array-utils:vector-push-extend-position value (value data) index)
      (vector-push-extend value (value data))))

(defmethod pop-element ((data vector-data) &optional index)
  (if index
      (array-utils:vector-pop-position (value data) index)
      (vector-pop (value data))))
