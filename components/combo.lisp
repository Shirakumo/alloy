#|
 This file is a part of Alloy
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.alloy)

(defclass combo (value-component)
  ((state :initform NIL :accessor state)))

(defgeneric value-set (data))
(define-observable (setf value-set) (set observable))

(defmethod initialize-instance :after ((combo combo) &key)
  (update-combo-items combo (value-set combo))
  (on (setf value-set) (set (data combo))
      (update-combo-items combo set)))

(defmethod activate :after ((radio radio))
  (setf (state radio) :selecting))

(defmethod exit :after ((radio radio))
  (setf (state radio) NIL))

(defmethod update-combo-items ((combo combo) set)
  )

(defclass combo-set (combo)
  ((value-set :initform (arg! :value-set) :initarg :value-set :accessor value-set)))
