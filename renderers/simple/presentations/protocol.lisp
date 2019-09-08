#|
 This file is a part of Alloy
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.alloy.renderers.simple.presentations)

(defclass renderer (simple:renderer)
  ())

(defclass shape ()
  ())

(defclass style (simple:style)
  ((simple:fill-color :initform NIL)
   (simple:font :initform NIL)
   (simple:font-size :initform NIL)
   (simple:line-width :initform NIL)
   (simple:fill-mode :initform NIL)
   (simple:composite-mode :initform NIL)
   (z-index :initarg :z-index :initform NIL :accessor z-index)
   (offset :initarg :offset :initform NIL :accessor offset)
   (scale :initarg :scale :initform NIL :accessor scale)
   (rotation :initarg :rotation :initform NIL :accessor rotation)
   (pivot :initarg :pivot :initform NIL :accessor pivot)))

(defun style (&rest args)
  (apply #'make-instance 'style args))

(defgeneric merge-style-into (target source))
(defgeneric activate-style (style renderer))

;; FIXME: change this to be RENDERABLE so that layouts can be styled too.
(stealth-mixin:define-stealth-mixin component () alloy:component
  ((shapes :initform (make-array 0 :adjustable T :fill-pointer T) :accessor shapes)))

(defgeneric realize-component (renderer component))
(defgeneric shape-style (renderer shape component))
(defgeneric clear-shapes (component))
(defgeneric find-shape (id component &optional errorp))
(defgeneric (setf find-shape) (shape id component))

(defmacro define-realisation ((renderer component) &body shapes)
  `(defmethod realize-component ((alloy:renderer ,renderer) (alloy:component ,component))
     (clear-shapes alloy:component)
     ,@(loop for ((name type) . initargs) in shapes
             collect `(setf (find-shape ',name alloy:component)
                            (make-instance ',type ,@initargs)))
     alloy:component))

(defmacro define-style ((renderer component) &body shapes)
  (let* ((default (find T shapes :key #'car))
         (shapes (if default (remove default shapes) shapes)))
    `(defmethod shape-style ((alloy:renderer ,renderer) (shape symbol) (alloy:component ,component))
       (flet ((alloy:focus ()
                (alloy:focus-for alloy:component alloy:renderer))
              (alloy:bounds ()
                (alloy:extent-for alloy:component alloy:renderer)))
         (declare (ignorable #'alloy:focus #'alloy:bounds))
         (case shape
           ,@(loop for (name . initargs) in shapes
                   collect `(,name (merge-style-into (call-next-method)
                                                     (style ,@initargs ,@default))))
           (T (call-next-method)))))))

(defmethod alloy:register :after ((component component) (renderer renderer))
  (realize-component renderer component))

(defmethod alloy:render-with ((renderer renderer) (element alloy:layout-element) (component component))
  (simple:with-pushed-transforms (renderer)
    (simple:translate renderer (alloy:bounds element))
    (loop for shape across (shapes component)
          for style = (shape-style renderer (car shape) component)
          do (simple:with-pushed-transforms (renderer)
               (simple:with-pushed-styles (renderer)
                 (activate-style style renderer)
                 (alloy:render-with (cdr shape) element renderer))))))

(defmethod merge-style-into ((target style) (source style))
  (loop for slot in '(simple:fill-color simple:font simple:font-size
                      simple:line-width simple:composite-mode
                      z-index offset scale rotation pivot)
        for src = (slot-value source slot)
        do (when src (setf (slot-value target slot) src)))
  target)

(defmethod activate-style ((style style) (renderer renderer))
  (setf (simple:fill-color renderer) (simple:fill-color style))
  (setf (simple:line-width renderer) (simple:line-width style))
  (setf (simple:font renderer) (simple:font style))
  (setf (simple:font-size renderer) (simple:font-size style))
  (setf (simple:composite-mode renderer) (simple:composite-mode style))
  ;; TODO: Not sure this is quite right.
  (setf (simple:z-index renderer) (z-index style))
  (simple:translate renderer (offset style))
  (simple:translate renderer (pivot style))
  (simple:rotate renderer (rotation style))
  (simple:scale renderer (scale style))
  (simple:translate renderer (alloy:point (- (alloy:point-x (pivot style)))
                                          (- (alloy:point-y (pivot style))))))

(defmethod shape-style ((renderer renderer) shape (component component))
  (style :fill-color (simple:color 0 0 0)
         :line-width 1.0
         :font (simple:request-font renderer :default)
         :font-size 12.0
         :composite-mode :source-over
         :z-index 0
         :offset (alloy:point 0 0)
         :scale (alloy:size 1 1)
         :rotation 0.0
         :pivot (alloy:point 0 0)))

(defmethod clear-shapes ((component component))
  (setf (fill-pointer (shapes component)) 0))

(defmethod find-shape (id (component component) &optional errorp)
  (or (cdr (find id (shapes component) :key #'car))
      (when errorp (error "No such shape~%  ~s~%in~%  ~s"
                          id component))))

(defmethod (setf find-shape) ((shape shape) id (component component))
  ;; FIXME: mark dirty somehow (mark-for-render component)
  (let ((record (find id (shapes component) :key #'car)))
    (if record
        (setf (cdr record) shape)
        (vector-push-extend (cons id shape) (shapes component)))
    shape))
