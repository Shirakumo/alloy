#|
 This file is a part of Alloy
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.alloy)

(defgeneric layout-tree (layout-element))
(defgeneric bounds (layout-element))
(defgeneric (setf bounds) (extent layout-element))
(defgeneric layout-element (component layout-tree))
(defgeneric notice-bounds (changed parent))
(defgeneric suggest-bounds (extent layout-element))

(defclass layout-element (element renderable)
  ((layout-tree :initform NIL :reader layout-tree)
   (parent :initarg :parent :initform (error "PARENT required.") :reader parent)
   (bounds :initform (extent) :reader bounds)))

(defmethod initialize-instance :after ((element layout-element) &key)
  (cond ((typep (parent element) 'layout-tree)
         (let ((layout-tree (parent element)))
           (setf (slot-value element 'layout-tree) layout-tree)
           (setf (slot-value element 'parent) NIL)
           (setf (root layout-tree) element)))
        (T
         (setf (slot-value element 'layout-tree) (layout-tree (parent element)))
         (enter element (parent element)))))

(defmethod print-object ((element layout-element) stream)
  (print-unreadable-object (element stream :type T :identity T)
    (format stream "~a" (bounds element))))

(defmethod (setf bounds) ((extent extent) (element layout-element))
  (let ((current (bounds element)))
    (setf (extent-x current) (extent-x extent))
    (setf (extent-y current) (extent-y extent))
    (setf (extent-w current) (extent-w extent))
    (setf (extent-h current) (extent-h extent))
    extent))

(defmethod (setf bounds) :after (extent (element layout-element))
  (when (parent element)
    (notice-bounds element (parent element))))

(defmethod x ((element layout-element)) (extent-x (bounds element)))
(defmethod y ((element layout-element)) (extent-y (bounds element)))
(defmethod w ((element layout-element)) (extent-w (bounds element)))
(defmethod h ((element layout-element)) (extent-h (bounds element)))

(defmethod handle ((event event) (element layout-element) ui))

(defmethod handle :around ((event pointer-event) (element layout-element) ui)
  (when (contained-p (point event) (bounds element))
    (call-next-method)))

(defclass layout-entry (layout-element)
  ((component :initarg :component :initform (error "COMPONENT required.") :reader component)))

(defmethod initialize-instance :after ((element layout-entry) &key)
  (associate element (component element) (layout-tree element)))

(defmethod register :after ((element layout-entry) (renderer renderer))
  (register (component element) renderer))

(defmethod handle ((event event) (element layout-entry) ui)
  (handle event (component element) ui))

(defmethod handle :after ((event pointer-down) (element layout-entry) ui)
  (activate (focus-element (component element) ui)))

(defmethod render ((renderer renderer) (element layout-entry))
  (render-with renderer element (component element)))

(defmethod suggest-bounds (extent (element layout-entry))
  (suggest-bounds extent (component element)))

(defclass layout (layout-element container)
  ())

(defmethod enter :before ((element layout-element) (layout layout) &key)
  (unless (eq layout (parent element))
    (error "Cannot enter~%  ~a~%into the layout~%  ~a~%as it has another parent~%  ~a"
           element layout (parent element))))

(defmethod enter :after ((element layout-element) (layout layout) &key)
  (notice-bounds element layout))

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
  (notice-bounds element layout))

(defmethod element-index :before ((element layout-element) (layout layout))
  (unless (eq layout (parent element))
    (error "The element~%  ~a~%is not in~%  ~a"
           element layout)))

(defmethod register :after ((layout layout) (renderer renderer))
  (do-elements (element layout)
    (register element renderer)))

(defmethod render ((renderer renderer) (layout layout))
  (do-elements (element layout)
    (render renderer element)))

(defmethod maybe-render ((renderer renderer) (layout layout))
  (do-elements (element layout)
    (maybe-render renderer element)))

(defmethod handle ((event event) (layout layout) ui)
  (do-elements (element layout)
    (when (handle event element ui)
      (return))))

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

(defmethod render ((renderer renderer) (tree layout-tree))
  (render renderer (root tree)))

(defmethod maybe-render ((renderer renderer) (tree layout-tree))
  (maybe-render renderer (root tree)))

(defmethod handle ((event pointer-event) (tree layout-tree) ui)
  (handle event (root tree) ui))
