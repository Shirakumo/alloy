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

(defmethod expand-place-data ((place cons))
  (expand-compound-place-data (first place) (rest place)))

(defmacro place-data (place)
  (expand-place-data place))

(defclass value-data (data)
  ((value :initarg :value :accessor value)))

(defgeneric value (data))
(defgeneric (setf value) (new-value data))
(make-observable '(setf value) '(new-value observable))

(defmethod value ((string string)) string)

(defmethod refresh ((data value-data))
  (notify-observers '(setf value) data (value data) data))

(defmethod expand-place-data (atom)
  `(make-instance 'value-data :value ,atom))

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
    `(make-instance 'place-data
                    :getter (lambda () (,place ,@args))
                    :setter (lambda (,value) (setf (,place ,@args) ,value)))))

(defmethod expand-place-data ((place symbol))
  (let ((value (gensym "VALUE")))
    `(make-instance 'place-data
                    :getter (lambda () ,place)
                    :setter (lambda (,value) (setf ,place ,value)))))

(defclass slot-data (value-data)
  ((object :initarg :object :initform (arg! :object) :accessor object)
   (slot :initarg :slot :initform (arg! :slot) :accessor slot)))

(defmethod value ((data slot-data))
  (slot-value (object data) (slot data)))

(defmethod (setf value) (new-value (data slot-data))
  (setf (slot-value (object data) (slot data)) new-value))

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
