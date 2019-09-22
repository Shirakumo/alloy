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

(defclass renderer ()
  ((allocated-p :initform NIL :reader allocated-p)))

(defmethod allocate :after ((renderer renderer))
  (setf (slot-value renderer 'allocated-p) T))

(defmethod deallocate :after ((renderer renderer))
  (setf (slot-value renderer 'allocated-p) NIL))

(defclass renderable ()
  ((render-needed-p :initform T :reader render-needed-p)
   (renderer :reader renderer)))

(defmethod initialize-instance :after ((renderable renderable) &key renderer)
  (when renderer (register renderable renderer)))

(defmethod register :before ((renderable renderable) (renderer renderer))
  (when (and (slot-boundp renderable 'renderer)
             (not (eql (renderer renderable) renderer)))
    (error 'renderable-already-registered
           :renderable renderable :renderer renderer)))

(defmethod register :after ((renderable renderable) (renderer renderer))
  (setf (slot-value renderable 'renderer) renderer))

(defmethod mark-for-render ((renderable renderable))
  (setf (slot-value renderable 'render-needed-p) T))

(defmethod render :after ((renderer renderer) (renderable renderable))
  (setf (slot-value renderable 'render-needed-p) NIL))

(defmethod maybe-render :around ((renderer renderer) (renderable renderable))
  (if (render-needed-p renderable)
      (render renderer renderable)
      (call-next-method)))

