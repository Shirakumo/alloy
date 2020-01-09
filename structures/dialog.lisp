#|
 This file is a part of Alloy
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.alloy)

(defclass dialog (structure)
  (window))

(defmethod accept :after ((dialog dialog))
  (leave dialog T))

(defmethod reject :after ((dialog dialog))
  (leave dialog T))

(defmethod enter ((element element) (dialog dialog) &rest args)
  (apply #'enter element (slot-value dialog 'window) args))

(defmethod update ((element element) (dialog dialog) &rest args)
  (apply #'update element (slot-value dialog 'window) args))

(defmethod leave ((element element) (dialog dialog))
  (leave element (slot-value dialog 'window)))

(defmethod initialize-instance :after ((dialog dialog) &key layout focus layout-parent focus-parent (accept "Ok") (reject "Cancel") (title "Notice") (size (size 300 200)))
  ;; KLUDGE: we use a fixed layout here to allow our dialog to be positioned anywhere regardless of the
  ;;         layout we're actually being entered into.
  ;;         This could be fixed by demanding that the root always have a fixed layout available for popus and such.
  (let ((stub-layout (make-instance 'fixed-layout :layout-parent layout-parent))
        (window (make-instance 'window :title title :minimizable NIL :maximizable NIL
                                       :layout layout :focus focus :focus-parent focus-parent))
        (layout (make-instance 'horizontal-linear-layout :align :end :cell-margins (margins 5 0)))
        (accept (represent accept 'button))
        (reject (represent reject 'button)))
    (setf (slot-value dialog 'window) window)
    (on close (window)
      (reject dialog))
    (on activate (accept)
      (accept dialog))
    (on activate (reject)
      (reject dialog))
    (when reject
      (enter reject layout))
    (when accept
      (enter accept layout))
    (enter layout (layout-element window) :place :south)
    (enter accept (focus-element window) :layer 2)
    (enter reject (focus-element window) :layer 2)
    (enter window stub-layout :w (w size) :h (h size))
    (finish-structure dialog stub-layout (focus-element window))))

(defclass dialog* (dialog)
  ((on-accept :initarg :on-accept :initform #'identity :accessor on-accept)
   (on-reject :initarg :on-reject :initform #'identity :accessor on-reject)))

(defmethod accept ((dialog dialog*))
  (funcall (on-accept dialog) dialog))

(defmethod reject ((dialog dialog*))
  (funcall (on-reject dialog) dialog))
