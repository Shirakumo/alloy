#|
 This file is a part of Alloy
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.alloy)

(defgeneric allocate (renderer))
(defgeneric deallocate (renderer))
(defgeneric allocated-p (renderer))
(defgeneric register (renderable renderer))

(defgeneric render-needed-p (renderable))
(defgeneric mark-for-render (renderable))
(defgeneric render (renderer renderable))
(defgeneric maybe-render (renderer renderable))
(defgeneric constrain-visibility (extent renderer))
(defgeneric reset-visibility (renderer))
(defgeneric extent-visible-p (extent renderer))
(defgeneric translate (renderer point))

;; TODO: Replace 'call with constrained visibility' with 'constrain-visibility' and 'clear-visibility'
;;       and then mandate that visibility constraints are only applicable within a RENDER call.
;;       thus combining the current dual-use of c-w-c-v and w-p-m.

(defclass renderer ()
  ((allocated-p :initform NIL :reader allocated-p)
   (visible-bounds :initform (%extent (%px 0f0) (%px 0f0) (%px 0f0) (%px 0f0)) :accessor visible-bounds)))

(defmethod constrain-visibility ((extent extent) (renderer renderer))
  (setf (visible-bounds renderer) (extent-intersection extent (visible-bounds renderer))))

(defmethod constrain-visibility ((size size) (renderer renderer))
  (constrain-visibility (extent 0 0 (size-w size) (size-h size)) renderer))

(defmethod reset-visibility ((renderer renderer))
  (setf (extent-x (visible-bounds renderer)) (px most-negative-single-float))
  (setf (extent-y (visible-bounds renderer)) (px most-negative-single-float))
  (setf (extent-w (visible-bounds renderer)) (px float-features:single-float-positive-infinity))
  (setf (extent-h (visible-bounds renderer)) (px float-features:single-float-positive-infinity)))

(defmethod translate :before ((renderer renderer) (point point))
  (let ((bounds (visible-bounds renderer)))
    (setf (extent-x bounds) (px (- (to-px (extent-x bounds)) (to-px (point-x point)))))
    (setf (extent-y bounds) (px (- (to-px (extent-y bounds)) (to-px (point-y point)))))))

(defmethod extent-visible-p ((extent extent) (renderer renderer))
  (overlapping-p extent (visible-bounds renderer)))

(defmethod allocate :after ((renderer renderer))
  (setf (slot-value renderer 'allocated-p) T))

(defmethod deallocate :after ((renderer renderer))
  (setf (slot-value renderer 'allocated-p) NIL))

(defmethod suggest-size :before (size (renderer renderer))
  (setf (extent-w (visible-bounds renderer)) (size-w size))
  (setf (extent-h (visible-bounds renderer)) (size-h size)))

(defmethod (setf bounds) :before (extent (renderer renderer))
  (setf (extent-w (visible-bounds renderer)) (size-w extent))
  (setf (extent-h (visible-bounds renderer)) (size-h extent)))

(defclass renderable ()
  ((render-needed-p :initform T :reader render-needed-p)))

(defmethod mark-for-render ((renderable renderable))
  (setf (slot-value renderable 'render-needed-p) T))

(defmethod render :after ((renderer renderer) (renderable renderable))
  (setf (slot-value renderable 'render-needed-p) NIL))

(defmethod maybe-render :around ((renderer renderer) (renderable renderable))
  (if (render-needed-p renderable)
      (render renderer renderable)
      (call-next-method)))

