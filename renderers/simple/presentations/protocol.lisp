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
  ((offset :initarg :offset :initform NIL :accessor offset)
   (scale :initarg :scale :initform NIL :accessor scale)
   (rotation :initarg :rotation :initform NIL :accessor rotation)
   (pivot :initarg :pivot :initform NIL :accessor pivot)))

(defun style (&rest args)
  (apply #'make-instance 'style args))

(defgeneric merge-style-into (target source))
(defgeneric activate-style (style renderer))

(stealth-mixin:define-stealth-mixin component () alloy:component
  ((shapes :initform () :accessor shapes)))

(defgeneric realise-component (renderer component))
(defgeneric shape-style (renderer shape component))
(defgeneric clear-shapes (component))
(defgeneric find-shape (id component &optional errorp))
(defgeneric (setf find-shape) (shape id component))

(defmethod alloy:register ((component component) (renderer renderer))
  (realise-component renderer component))

(defmethod alloy:render-with ((renderer renderer) (element alloy:layout-element) (component component))
  (simple:with-pushed-transforms (renderer)
    (simple:translate renderer (alloy:extent element))
    (loop for shape in (shapes renderer)
          for style = (shape-style renderer shape component)
          do (simple:with-pushed-transforms (renderer)
               (simple:with-pushed-styles (renderer)
                 (activate-style style renderer)
                 (render shape renderer))))))

(defmethod merge-style-into ((target style) (source style))
  (loop for slot in '(fill-color line-width font font-size offset scale rotation pivot)
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
  (simple:translate renderer (offset style))
  (simple:translate renderer (pivot style))
  (simple:rotate renderer (rotation style))
  (simple:scale renderer (scale style))
  (simple:translate renderer (alloy:point (- (alloy:point-x (pivot style)))
                                          (- (alloy:point-y (pivot style))))))

(defmethod shape-style ((renderer renderer) (shape shape) (component component))
  (style :fill-color (simple:color 0 0 0)
         :line-width 1.0
         :font :default
         :font-size 12.0
         :composite-mode :source-over
         :offset (alloy:point 0 0)
         :scale (alloy:point 1 1)
         :rotation 0.0
         :pivot (alloy:point 0 0)))

(defmethod clear-shapes ((component component))
  (setf (shapes component) ()))

(defmethod find-shape (id (component component) &optional errorp)
  (or (cdr (assoc id (shapes component)))
      (when errorp (error "No such shape~%  ~s~%in~%  ~s"
                          id component))))

(defmethod (setf find-shape) ((shape shape) id (component component))
  ;; FIXME: mark dirty somehow (mark-for-render component)
  (let ((record (assoc id (shapes component))))
    (if record
        (setf (cdr record) shape)
        (push (cons id shape) (shapes component)))))
