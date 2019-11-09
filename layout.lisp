#|
 This file is a part of Alloy
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.alloy)

(defgeneric layout-tree (layout-element))
(defgeneric layout-parent (layout-element))
(defgeneric bounds (layout-element))
(defgeneric (setf bounds) (extent layout-element))
(defgeneric notice-bounds (changed parent))
(defgeneric suggest-bounds (extent layout-element))
(defgeneric ensure-visible (extent parent))

(defclass layout-element (element)
  ((layout-tree :initform NIL :reader layout-tree)
   (layout-parent :initarg :layout-parent :reader layout-parent)
   (bounds :initform (extent) :accessor bounds)))

(defmethod initialize-instance :after ((element layout-element) &key)
  (when (slot-boundp element 'layout-parent)
    (etypecase (layout-parent element)
      (layout-tree
       (let ((layout-tree (layout-parent element)))
         (setf (slot-value element 'layout-tree) layout-tree)
         (setf (slot-value element 'layout-parent) element)
         (setf (root layout-tree) element)))
      (layout-element
       (setf (slot-value element 'layout-tree) (layout-tree (layout-parent element)))
       (enter element (layout-parent element))))))

(defmethod print-object ((element layout-element) stream)
  (print-unreadable-object (element stream :type T :identity T)
    (format stream "~a" (bounds element))))

(defmethod x ((element layout-element)) (extent-x (bounds element)))
(defmethod y ((element layout-element)) (extent-y (bounds element)))
(defmethod w ((element layout-element)) (extent-w (bounds element)))
(defmethod h ((element layout-element)) (extent-h (bounds element)))

(defmethod handle ((event event) (element layout-element) ui)
  (decline))

(defmethod render :around ((renderer renderer) (element layout-element))
  (when (extent-visible-p (bounds element) renderer)
    (with-unit-parent element
      (call-next-method))))

(defmethod ensure-visible ((extent extent) (element layout-element))
  (unless (eq element (layout-parent element))
    (ensure-visible extent element)))

(defclass layout (layout-element container renderable)
  ())

(defmethod enter :after ((element layout-element) (layout layout) &key)
  (notice-bounds element layout))

(defmethod enter :before ((element layout-element) (parent layout) &key)
  (cond ((not (slot-boundp element 'layout-parent))
         (setf (slot-value element 'layout-tree) (layout-tree parent))
         (setf (slot-value element 'layout-parent) parent))
        ((not (eq parent (layout-parent element)))
         (error 'element-has-different-parent
                :element element :container parent :parent (layout-parent element)))))

(defmethod leave :before ((element layout-element) (parent layout))
  (unless (eq parent (layout-parent element))
    (error 'element-has-different-parent
           :element element :container parent :parent (layout-parent element))))

(defmethod leave :after ((element layout-element) (parent layout))
  (slot-makunbound element 'layout-tree)
  (slot-makunbound element 'layout-parent))

(defmethod update :after ((element layout-element) (layout layout) &key)
  (notice-bounds element layout))

(defmethod element-index :before ((element layout-element) (layout layout))
  (unless (eq layout (layout-parent element))
    (error 'element-not-contained
           :element element :container layout)))

(defmethod register :after ((layout layout) (renderer renderer))
  (do-elements (element layout)
    (register element renderer)))

(defmethod render ((renderer renderer) (layout layout))
  (do-elements (element layout)
    (render renderer element)))

(defmethod maybe-render ((renderer renderer) (layout layout)))

(defmethod maybe-render :after ((renderer renderer) (layout layout))
  (do-elements (element layout)
    (maybe-render renderer element)))

(defmethod handle ((event pointer-event) (layout layout) ui)
  (do-elements (element layout :result (decline))
    (when (contained-p (location event) (bounds element))
      (handle event element ui)
      (return))))

(defclass layout-tree ()
  ((root :initform NIL :accessor root)
   (ui :initarg :ui :reader ui)))

(defmethod (setf root) :before ((element layout-element) (tree layout-tree))
  (when (root tree)
    (error 'root-already-established
           :element element :tree tree)))

(defmethod register ((tree layout-tree) (renderer renderer))
  (register (root tree) renderer))

(defmethod render ((renderer renderer) (tree layout-tree))
  (render renderer (root tree)))

(defmethod maybe-render ((renderer renderer) (tree layout-tree))
  (maybe-render renderer (root tree)))

(defmethod handle ((event pointer-event) (tree layout-tree) ui)
  (unless (handle event (root tree) ui)
    (decline)))

(defmethod suggest-bounds (extent (tree layout-tree))
  (let ((root (root tree)))
    (suggest-bounds extent root)
    (setf (bounds root) extent)))
