#|
 This file is a part of Alloy
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.alloy)

(defclass text-component (component)
  ((text :initarg :text :initform "" :accessor text)))

(defclass image-component (component)
  ((image :initarg :image :initform NIL :accessor image)))

(defclass interactable-component (component)
  ())

(defmethod handle ((event pointer-move) (component interactable-component) ctx)
  (unless (eql :strong (focus-for component ctx))
    (setf (focus-for component ctx) :weak)))

(defmethod (setf focus) :after (value (component interactable-component))
  (mark-for-render component))

(defclass label (text-component)
  ())

(defmethod enter ((string string) (layout layout) &rest args)
  (apply #'enter (make-instance 'label :text string) layout args))

(defclass icon (image-component)
  ())

(defclass button (text-component image-component interactable-component)
  ((pressed :initform NIL :accessor pressed)))

(defmethod (setf pressed) :after (value (button button))
  (mark-for-render button))

(defmethod handle ((event pointer-down) (button button) ctx)
  (setf (pressed button) T))

(defmethod handle ((event pointer-up) (button button) ctx)
  (setf (pressed button) NIL)
  (setf (focus-for button ctx) :weak))

(defmethod handle ((event button-down) (button button) ctx)
  (case (button event)
    (:a (setf (pressed button) T))
    (T (call-next-method))))

(defmethod handle ((event button-up) (button button) ctx)
  (case (button event)
    (:a (setf (pressed button) NIL))
    (T (call-next-method))))

(defmethod exit :after ((button button))
  (setf (pressed button) NIL))

(defclass switch (interactable-component) ())

(defgeneric state (switch))
(defgeneric (setf state) (value switch))

(defmethod (setf state) :after (value (switch switch))
  (mark-for-render switch))

(defmethod activate ((switch switch))
  (setf (state switch) (not (state switch))))

;; TODO: combobox
(defclass combobox (interactable-component)
  ())


(defclass switch* (switch)
  ((state :initarg :state :initform NIL :accessor state)))

(defclass input-line* (input-line)
  ((text :initform (make-array 0 :adjustable T :fill-pointer T :element-type 'character) :accessor text)))

(defclass input-box* (input-box)
  ((text :initform (make-array 0 :adjustable T :fill-pointer T :element-type 'character) :accessor text)))
