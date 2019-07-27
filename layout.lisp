#|
 This file is a part of Alloy
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.alloy)

(defgeneric layout-tree (layout-element))
(defgeneric extent (layout-element))
(defgeneric (setf extent) (extent layout-element))
(defgeneric layout-element (component layout-tree))
(defgeneric notice-extent (changed parent))

(defclass layout-element (element renderable)
  ((layout-tree :initform NIL :reader layout-tree)
   (parent :initarg :parent :initform (error "PARENT required.") :reader parent)
   (extent :initform (make-extent) :reader extent)))

(defmethod initialize-instance :after ((element layout-element) &key)
  (cond ((typep (parent element) 'layout-tree)
         (let ((layout-tree (parent element)))
           (setf (slot-value element 'layout-tree) layout-tree)
           (setf (slot-value element 'parent) NIL)
           (setf (root (parent element)) element)))
        (T
         (setf (slot-value element 'layout-tree) (layout-tree (parent element)))
         (enter element (parent element)))))

(defmethod (setf extent) ((extent extent) (element layout-element))
  (let ((current (extent element)))
    (setf (extent-x current) (extent-x extent))
    (setf (extent-y current) (extent-y extent))
    (setf (extent-w current) (extent-w extent))
    (setf (extent-h current) (extent-h extent))
    (extent)))

(defmethod (setf extent) :after (extent (element layout-element))
  (notice-extent element (parent element)))

(defmethod x ((element layout-element)) (extent-x (extent element)))
(defmethod y ((element layout-element)) (extent-y (extent element)))
(defmethod w ((element layout-element)) (extent-w (extent element)))
(defmethod h ((element layout-element)) (extent-h (extent element)))

(defclass layout-entry (layout-element)
  ((component :initarg :component :initform (error "COMPONENT required.") :reader component)))

(defmethod initialize-instance :after ((element layout-entry) &key)
  (associate element (component element) (layout-tree element)))

(defmethod register :after ((element layout-entry) (renderer renderer))
  (register (component element) renderer))

(defmethod render ((renderer renderer) (element layout-entry) ui)
  (render renderer (component element) ui))

(defmethod maybe-render ((element layout-entry) (renderer renderer) ui)
  (maybe-render (component element) renderer ui))

(defclass layout (layout-element container)
  ())

(defmethod enter :before ((element layout-element) (layout layout) &key)
  (unless (eq layout (parent element))
    (error "Cannot enter~%  ~a~%into the layout~%  ~a~%as it has another parent~%  ~a"
           element layout (parent element))))

(defmethod enter :after ((element layout-element) (layout layout) &key)
  (notice-extent element layout))

(defmethod leave :before ((element layout-element) (layout layout))
  (unless (eq layout (parent element))
    (error "Cannot leave~%  ~a~%from the layout~%  ~a~%as it has another parent~%  ~a"
           element layout (parent element))))

(defmethod leave :after ((element layout-entry) (layout layout))
  (disassociate element (component element) (layout-tree layout)))

(defmethod leave ((component component) (layout layout))
  (leave (layout-element component (layout-tree layout)) layout))

(defmethod update ((component component) (layout layout) &rest args)
  (apply #'update (layout-element component (layout-tree layout)) layout args))

(defmethod update :after ((element layout-element) (layout layout) &key)
  (notice-extent element layout))

(defmethod register :after ((layout layout) (renderer renderer))
  (do-elements (element layout)
    (register element renderer)))

(defmethod render ((renderer renderer) (layout layout) ui)
  (do-elements (element layout)
    (render renderer element ui)))

(defmethod maybe-render ((layout layout) (renderer renderer) ui)
  (do-elements (element layout)
    (maybe-render element renderer ui)))

(defclass layout-tree (element-table)
  ((root :initform NIL :accessor root)))

(defmethod (setf root) :before ((element layout-element) (tree layout-tree))
  (when (root tree)
    (error "Cannot set~%  ~a~%as the root of~%  ~a~%as it already has a root in~%  ~a"
           element tree (root tree))))

(defmethod layout-element ((component component) (tree layout-tree))
  (associated-element component tree))

(defmethod register ((tree layout-tree) (renderer renderer))
  (register (root tree) renderer))

(defmethod render ((renderer renderer) (tree layout-tree) ui)
  (render renderer (root tree) ui))

(defmethod maybe-render ((tree layout-tree) (renderer renderer) ui)
  (maybe-render (root tree) renderer ui))
