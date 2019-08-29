#|
 This file is a part of Alloy
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.alloy.layouts.constraint)

(defun make-variables (element solver &key (strength :medium))
  (list (cass:make-variable solver :name (format NIL "X ~a" element) :strength strength)
        (cass:make-variable solver :name (format NIL "Y ~a" element) :strength strength)
        (cass:make-variable solver :name (format NIL "W ~a" element) :strength strength)
        (cass:make-variable solver :name (format NIL "H ~a" element) :strength strength)))

(defclass layout (alloy:layout alloy:layout-element)
  ((solver :initform (cass:make-solver) :reader solver)
   (variables :initform (make-hash-table :test 'eq) :reader variables)
   (constraints :initform (make-hash-table :test 'eq) :reader constraints)))

(defmethod initialize-instance :after ((layout layout) &key)
  (setf (gethash layout (variables layout))
        (make-variables layout (solver layout) :strength :strong)))

(defmacro with-vars ((x y w h layout) element &body body)
  `(destructuring-bind (,x ,y ,w ,h)
       (gethash ,element (variables ,layout))
     ,@body))

(defun suggest (layout element extent)
  (with-vars (x y w h layout) element
    (cass:suggest x (alloy:extent-x extent))
    (cass:suggest y (alloy:extent-x extent))
    (cass:suggest w (alloy:extent-w extent))
    (cass:suggest h (alloy:extent-h extent))
    (cass:update-variables layout)))

(defun update (layout element)
  (with-vars (x y w h layout) element
    (setf (alloy:bounds element) (alloy:extent (cass:value x) (cass:value y) (cass:value w) (cass:value h)))))

(defmethod alloy:enter ((element alloy:layout-element) (layout layout) &key constraints)
  (setf (gethash element (variables layout))
        (make-variables element (solver layout)))
  (apply-constraints constraints element layout))

(defmethod alloy:leave ((element alloy:layout-element) (layout layout))
  (remhash element (variables layout)))

(defmethod alloy:update ((element alloy:layout-element) (layout layout) &key constraints)
  (apply-constraints constraints element layout))

(defmethod alloy:notice-bounds ((element alloy:layout-element) (layout layout))
  ;; This may lead to all elements being constrained immediately.
  (suggest layout element (alloy:extent element))
  (alloy:do-elements (element layout)
    (update layout element)))

(defmethod (setf alloy:bounds) :after (extent (layout layout))
  (suggest layout layout extent)
  (alloy:do-elements (element layout)
    (update layout element)))

(defmethod alloy:suggest-bounds (extent (layout layout))
  (suggest layout layout extent)
  (with-vars (x y w h layout) layout
    (alloy:extent (cass:value x) (cass:value y) (cass:value w) (cass:value h))))

(defun rewrite-variable (var element layout)
  (with-vars (rx ry rw rh layout) layout
    (with-vars (x y w h layout) element
      (case var
        (:x x)
        (:y y)
        (:w w)
        (:h h)
        (:l `(- x rx))
        (:b `(- y ry))
        (:r `(- (+ ,rx ,rw) (+ ,x ,w)))
        (:u `(- (+ ,ry ,rh) (+ ,y ,h)))
        (:rx rx)
        (:ry ry)
        (:rw rw)
        (:rh rh)
        (T var)))))

(defun apply-contsraints (constraints element layout)
  (dolist (constraint (gethash element (constraints layout)))
    (cass:delete-constraint constraint))
  (dolist (constraint constraints)
    (destructuring-bind (expression &key (strength :required)) constraint
      (let ((constraint (cass:make-constraint (solver layout) :strength strength)))
        ))))
