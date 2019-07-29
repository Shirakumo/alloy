#|
 This file is a part of Alloy
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.alloy)

(defgeneric extent-for (component ui))
(defgeneric focus-for (component ui))

;;; Provided by the backend
(defgeneric clipboard (ui))
(defgeneric (setf clipboard) (content ui))
(defgeneric cursor (ui))
(defgeneric (setf cursor) (cursor ui))

(defclass ui (renderer)
  ((layout-tree :initarg :layout-tree :reader layout-tree)
   (focus-tree :initarg :focus-tree :reader focus-tree)))

(defmethod layout-element ((component component) (ui ui))
  (layout-element component (layout-tree ui)))

(defmethod focus-element ((component component) (ui ui))
  (focus-element component (focus-tree ui)))

(defmethod extent-for ((component component) (ui ui))
  (extent (layout-element component (layout-tree ui))))

(defmethod focus-for ((component component) (ui ui))
  (focus (focus-element component (focus-tree ui))))

(defmethod handle ((event direct-event) (all (eql T)) (ui ui))
  (handle event (focus-tree ui) ui))

(defmethod handle ((event pointer-event) (all (eql T)) (ui ui))
  (handle event (layout-tree ui) ui))

(defmethod render ((ui ui) (thing (eql T)))
  (render ui (layout-tree ui)))

(defmethod maybe-render ((ui ui) (thing (eql T)))
  (maybe-render ui (layout-tree ui)))

(defmethod activate ((ui ui))
  (mark-for-render (root (layout-tree ui))))
