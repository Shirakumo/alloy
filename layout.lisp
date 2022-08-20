#|
 This file is a part of Alloy
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.alloy)

(defgeneric layout-tree (layout-element))
(defgeneric layout-parent (layout-element))
(defgeneric notice-size (changed parent))
(defgeneric suggest-size (size layout-element))
(defgeneric preferred-size (layout-element))
(defgeneric ensure-visible (element parent))
(defgeneric bounds (layout-element))
(defgeneric (setf bounds) (extent layout-element))
(defgeneric resize (element w h))
(defgeneric location (element))
(defgeneric (setf location) (location element))
(defgeneric global-location (layout-element))
(defgeneric (setf global-location) (extent layout-element))

(defclass layout-element (element)
  ((layout-tree :initform NIL :reader layout-tree :writer set-layout-tree)
   (layout-parent :reader layout-parent)
   (bounds :initform (%extent (px 0) (px 0) (px 0) (px 0)) :reader bounds)))

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

(defmethod (setf x) ((unit unit) (element layout-element))
  (setf (extent-x (bounds element)) unit))
(defmethod (setf y) ((unit unit) (element layout-element))
  (setf (extent-y (bounds element)) unit))
(defmethod (setf w) ((unit unit) (element layout-element))
  (setf (extent-w (bounds element)) unit))
(defmethod (setf h) ((unit unit) (element layout-element))
  (setf (extent-h (bounds element)) unit))

(defmethod (setf x) ((real real) (element layout-element))
  (setf (extent-x (bounds element)) (px real)))
(defmethod (setf y) ((real real) (element layout-element))
  (setf (extent-y (bounds element)) (px real)))
(defmethod (setf w) ((real real) (element layout-element))
  (setf (extent-w (bounds element)) (px real)))
(defmethod (setf h) ((real real) (element layout-element))
  (setf (extent-h (bounds element)) (px real)))

(defmethod (setf bounds) ((extent extent) (element layout-element))
  (let ((bounds (bounds element)))
    (setf (extent-x bounds) (extent-x extent))
    (setf (extent-y bounds) (extent-y extent))
    (setf (extent-w bounds) (extent-w extent))
    (setf (extent-h bounds) (extent-h extent))
    extent))

(defmethod (setf bounds) ((size size) (element layout-element))
  (let ((bounds (bounds element)))
    (setf (extent-w bounds) (size-w size))
    (setf (extent-h bounds) (size-h size))
    size))

(defmethod location ((element layout-element))
  (point (extent-x (bounds element)) (extent-y (bounds element))))

(defmethod (setf location) ((point point) (element layout-element))
  (setf (extent-x (bounds element)) (point-x point))
  (setf (extent-y (bounds element)) (point-y point))
  point)

(defmethod resize ((element layout-element) (w unit) (h unit))
  (setf (extent-w (bounds element)) w)
  (setf (extent-h (bounds element)) h)
  element)

;; Default to minimal size
(defmethod preferred-size ((element layout-element))
  (suggest-size (size) element))

(defmethod suggest-size :around ((size size) (element layout-element))
  ;; No need to actually pass through if we're re-using the current size,
  ;; as it's already been stabilised before.
  (if (and (u= (size-w size) (extent-w (bounds element)))
           (u= (size-h size) (extent-h (bounds element))))
      size
      (call-next-method)))

(defmethod (setf bounds) :around ((extent extent) (element layout-element))
  ;; No need to actually pass through if we're re-using the current size,
  ;; as it's already been stabilised before.
  (let ((bounds (bounds element)))
    (if (and (u= (extent-w extent) (extent-w bounds))
             (u= (extent-h extent) (extent-h bounds)))
        (setf (extent-x bounds) (extent-x extent)
              (extent-y bounds) (extent-y extent))
        (call-next-method))))

(defmethod (setf bounds) :around ((size size) (element layout-element))
  ;; No need to actually pass through if we're re-using the current size,
  ;; as it's already been stabilised before.
  (let ((bounds (bounds element)))
    (when (or (u/= (size-w size) (extent-w bounds))
              (u/= (size-h size) (extent-h bounds)))
      (call-next-method))))

(defmethod compute-global-position ((element layout-element))
  (with-unit-parent element
    (let ((x 0f0) (y 0f0))
      (loop for current = element then parent
            for parent = (layout-parent current)
            do (incf x (pxx (bounds current)))
               (incf y (pxy (bounds current)))
               (when (typep current 'clip-view)
                 (incf x (pxx (offset current)))
                 (incf y (pxy (offset current))))
            until (eql current parent))
      (values x y))))

(defmacro with-global-bounds ((bounds element) &body body)
  (let ((x (gensym "X"))
        (y (gensym "Y")))
    `(multiple-value-bind (,x ,y) (compute-global-position element)
       (let* ((,x (%px ,x))
              (,y (%px ,y))
              (,bounds (bounds ,element))
              (,bounds (%extent ,x ,y (extent-w ,bounds) (extent-h ,bounds))))
         (declare (dynamic-extent ,x ,y ,bounds))
         ,@body))))

(defmethod global-location ((element layout-element))
  (multiple-value-bind (x y) (compute-global-position element)
    (px-point x y)))

(defmethod (setf global-location) ((location point) (element layout-element))
  (with-unit-parent element
    (multiple-value-bind (x y) (compute-global-position element)
      (let* ((parent-x (- x (pxx element)))
             (parent-y (- y (pyy element)))
             (new-parent-x (- (pxx location) parent-x))
             (new-parent-y (- (pxy location) parent-y)))
        (setf (extent-x (bounds element)) (px new-parent-x))
        (setf (extent-y (bounds element)) (py new-parent-y))
        location))))

(defmethod contained-p (thing (element layout-element))
  (with-global-bounds (bounds element)
    (contained-p thing bounds)))

(defmethod call-with-constrained-visibility (function (element layout-element) (renderer renderer))
  (let ((bounds (%extent (px 0) (px 0) (w element) (h element))))
    (declare (dynamic-extent bounds))
    (call-with-constrained-visibility function bounds renderer)))

(defmethod set-layout-tree :before (tree (element layout-element))
  (when (and (layout-tree element) tree (not (eq tree (layout-tree element))))
    (error 'element-has-different-root
           :bad-element element :container tree)))

(defmethod handle ((event event) (element layout-element))
  (decline))

(defmethod handle :around ((event event) (element layout-element))
  (with-unit-parent element
    (call-next-method)))

(defmethod render :around ((renderer renderer) (element layout-element))
  (with-unit-parent element
    (with-global-bounds (bounds element)
      (when (and (layout-tree element)
                 (extent-visible-p bounds renderer))
        (call-next-method)))))

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
  (when (slot-boundp element 'layout-parent)
    (leave element (layout-parent element))))

(defclass layout (layout-element container renderable)
  ())

(defmethod set-layout-tree :before (value (layout layout))
  (do-elements (element layout)
    (set-layout-tree value element)))

(defmethod enter :after ((element layout-element) (layout layout) &key)
  (when (layout-tree layout)
    (notice-size element layout)))

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
    (notice-size element layout)))

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

(defmethod clear ((tree layout-tree))
  (setf (root tree) NIL)
  (clear (popups tree)))

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

(defmethod suggest-size (size (tree layout-tree))
  (let ((root (root tree))
        (popups (popups tree)))
    (suggest-size size root)
    (setf (bounds root) size)
    (setf (bounds popups) (bounds popups))))
