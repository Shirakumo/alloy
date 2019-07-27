#|
 This file is a part of Alloy
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.alloy)

(defgeneric extent-for (component ui))
(defgeneric focus-for (component ui))

(defclass ui ()
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

(defmethod handle (event (ui ui))
  (handle event (focus-tree ui)))

(defmethod render ((renderer renderer) (all (eql T)) (ui ui))
  (render renderer (layout-tree ui) ui))

(defmethod render ((renderer renderer) (ui ui) (all (eql T)))
  (render renderer (layout-tree ui) ui))

(defmethod maybe-render ((renderer renderer) (all (eql T)) (ui ui))
  (maybe-render renderer (layout-tree ui) ui))

(defmethod maybe-render ((renderer renderer) (ui ui) (all (eql T)))
  (maybe-render renderer (layout-tree ui) ui))

(defmethod activate ((ui ui))
  (mark-for-render (root (layout-tree ui))))
