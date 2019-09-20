#|
 This file is a part of Alloy
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.alloy)

(defclass data (observable)
  ())

(defgeneric expand-place-data (place))
(defgeneric expand-compound-place-data (place args))

(defmethod expand-place-data ((cons place))
  (expand-compound-place-data (first cons) (rest cons)))

(defclass value-data (data)
  ((value :initarg :value :accessor value)))

(defgeneric value (data))
(defgeneric (setf value) (new-value data))
(make-observable '(setf value) '(new-value observable))

;;; General case.
(defclass place-data (data)
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

(defclass variable-data (data)
  ((variable :initarg :variable :initform (arg! :variable) :accessor variable)))

(defmethod value ((data variable-data))
  (symbol-value (variable data)))

(defmethod (setf value) (new-value (data variable-data))
  (setf (symbol-value (variable data)) new-value))

;; FIXME: this will not work correctly with lexical variables.
(defmethod expand-place-data ((place symbol))
  `(make-instance 'variable-data :variable ',place))

(defclass slot-data (data)
  ((object :initarg :object :initform (arg! :object) :accessor object)
   (slot :initarg :slot :initform (arg! :slot) :accessor slot)))

(defmethod value ((data slot-data))
  (slot-value (slot-data-object data) (slot-data-slot data)))

(defmethod (setf value) (new-value (data slot-data))
  (setf (slot-value (slot-data-object data) (slot-data-slot data)) new-value))

(defmethod expand-compound-place-data ((place (eql 'slot-value)) args)
  (destructuring-bind (object slot) body
    `(make-instance 'slot-data :object ,object :slot ,slot)))

(defclass aref-data (data)
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
