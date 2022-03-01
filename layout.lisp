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
(defgeneric ensure-visible (element parent))

;;; TODO: How do we resize a layout to its preferred size?
;;;       Using a (suggest-bounds (extent) ..) will give us a minimal
;;;       bound, not a preferred bound.

(defclass layout-element (element)
  ((layout-tree :initform NIL :reader layout-tree :writer set-layout-tree)
   (layout-parent :reader layout-parent)
   (bounds :initform (extent) :accessor bounds)))

(defmethod initialize-instance :after ((element layout-element) &key layout-parent)
  (when layout-parent
    (enter element layout-parent)))

(defmethod print-object ((element layout-element) stream)
  (print-unreadable-object (element stream :type T :identity T)
    (format stream "~a" (bounds element))))

(defmethod ui ((element layout-element))
  (ui (layout-tree element)))

(defmethod x ((element layout-element)) (extent-x (bounds element)))
(defmethod y ((element layout-element)) (extent-y (bounds element)))
(defmethod w ((element layout-element)) (extent-w (bounds element)))
(defmethod h ((element layout-element)) (extent-h (bounds element)))

(defmethod contained-p (thing (element layout-element))
  (contained-p thing (bounds element)))

(defmethod set-layout-tree :before (tree (element layout-element))
  (when (and (layout-tree element) tree (not (eq tree (layout-tree element))))
    (error 'element-has-different-root
           :bad-element element :container tree)))

(defmethod handle ((event event) (element layout-element))
  (decline))

(defmethod render :around ((renderer renderer) (element layout-element))
  (when (and (extent-visible-p (bounds element) renderer)
             (layout-tree element))
    (with-unit-parent element
      (call-next-method))))

(defmethod register :around ((element layout-element) (renderer renderer))
  (with-unit-parent element
    (call-next-method)))

(defmethod ensure-visible (element (parent layout-element))
  (when (and (slot-boundp parent 'layout-parent)
             (not (eq parent (layout-parent parent))))
    (ensure-visible element (layout-parent parent))))

(defmethod ensure-visible ((element layout-element) (parent (eql T)))
  (when (slot-boundp element 'layout-parent)
    (ensure-visible element (layout-parent element))))

(defmethod leave ((element layout-element) (parent (eql T)))
  (leave element (layout-parent element)))

(defclass layout (layout-element container renderable)
  ())

(defmethod set-layout-tree :before (value (layout layout))
  (do-elements (element layout)
    (set-layout-tree value element)))

(defmethod enter :after ((element layout-element) (layout layout) &key)
  (when (layout-tree layout)
    (notice-bounds element layout)))

(defmethod enter :before ((element layout-element) (parent layout) &key)
  (cond ((not (slot-boundp element 'layout-parent))
         (set-layout-tree (layout-tree parent) element)
         (setf (slot-value element 'layout-parent) parent))
        ((not (eq parent (layout-parent element)))
         (restart-case
             (error 'element-has-different-parent
                    :bad-element element :container parent :parent (layout-parent element))
           (reparent ()
             :report "Leave the element from its current parent."
             (leave element T))))
        (T
         (error 'element-already-contained
                :bad-element element :container parent))))

(defmethod leave :before ((element layout-element) (parent layout))
  (unless (eq parent (layout-parent element))
    (error 'element-has-different-parent
           :bad-element element :container parent :parent (layout-parent element))))

(defmethod leave :after ((element layout-element) (parent layout))
  (set-layout-tree NIL element)
  (slot-makunbound element 'layout-parent))

(defmethod update :after ((element layout-element) (layout layout) &key)
  (when (layout-tree layout)
    (notice-bounds element layout)))

(defmethod element-index :before ((element layout-element) (layout layout))
  (unless (eq layout (layout-parent element))
    (error 'element-not-contained
           :bad-element element :container layout)))

(defmethod register :after ((layout layout) (renderer renderer))
  (do-elements (element layout)
    (register element renderer)))

(defmethod render ((renderer renderer) (layout layout))
  (do-elements (element layout)
    (render renderer element)))

(defmethod maybe-render ((renderer renderer) (layout layout))
  (do-elements (element layout)
    (maybe-render renderer element)))

(defmethod handle ((event pointer-event) (layout layout))
  ;; Need to process in reverse order to ensure overlapping elements come first,
  ;; since elements drawn last overlap previous elements.
  (do-elements (element layout :result (decline) :from-end T)
    (when (contained-p (location event) element)
      (if (handle event element)
          (return)
          (decline)))))

(defclass layout-tree ()
  ((root :initform NIL :accessor root)
   (popups :initform (make-instance 'fixed-layout) :reader popups)
   (ui :initarg :ui :reader ui)))

(defmethod initialize-instance :after ((tree layout-tree) &key)
  (set-layout-tree tree (popups tree))
  (setf (slot-value (popups tree) 'layout-parent) (popups tree)))

(defmethod enter ((element layout-element) (tree layout-tree) &key force)
  (when (next-method-p) (call-next-method))
  (when force (setf (root tree) NIL))
  (setf (root tree) element))

(defmethod (setf root) :before ((none null) (tree layout-tree))
  (when (root tree)
    (set-layout-tree NIL (root tree))))

(defmethod (setf root) :before ((element layout-element) (tree layout-tree))
  (when (root tree)
    (error 'root-already-established
           :bad-element element :tree tree)))

(defmethod (setf root) :after ((element layout-element) (tree layout-tree))
  (set-layout-tree tree element)
  (setf (slot-value element 'layout-parent) element))

(defmethod register ((tree layout-tree) (renderer renderer))
  (register (root tree) renderer)
  (register (popups tree) renderer))

(defmethod render ((renderer renderer) (tree layout-tree))
  (render renderer (root tree))
  (render renderer (popups tree)))

(defmethod maybe-render ((renderer renderer) (tree layout-tree))
  (maybe-render renderer (root tree))
  (maybe-render renderer (popups tree)))

(defmethod handle ((event pointer-event) (tree layout-tree))
  (or (handle event (popups tree))
      (handle event (root tree))
      (decline)))

(defmethod suggest-bounds (extent (tree layout-tree))
  (let ((root (root tree))
        (popups (popups tree)))
    (suggest-bounds extent root)
    (setf (bounds root) extent)
    (setf (bounds popups) (bounds popups))))
