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

(stealth-mixin:define-stealth-mixin renderable () alloy:renderable
  ((shapes :initform (make-array 0 :adjustable T :fill-pointer T) :accessor shapes)))

(defgeneric realize-renderable (renderer renderable))
(defgeneric shape-style (renderer shape renderable))
(defgeneric clear-shapes (renderable))
(defgeneric find-shape (id renderable &optional errorp))
(defgeneric (setf find-shape) (shape id renderable))

(defmacro define-realisation ((renderer renderable) &body shapes)
  `(defmethod realize-renderable ((alloy:renderer ,renderer) (alloy:renderable ,renderable))
     (clear-shapes alloy:renderable)
     ,@(loop for shape in shapes
             collect (destructuring-bind ((name type &key when) &body initargs) shape
                       `(when ,(or when T)
                          (setf (find-shape ',name alloy:renderable)
                                (make-instance ',type ,@initargs)))))
     alloy:renderable))

(defmacro define-style ((renderer renderable) &body shapes)
  (let* ((default (find T shapes :key #'car))
         (shapes (if default (remove default shapes) shapes)))
    `(defmethod shape-style ((alloy:renderer ,renderer) (shape symbol) (alloy:renderable ,renderable))
       (flet ((alloy:focus (&optional thing)
                (if thing
                    (alloy:focus thing)
                    (alloy:focus-for alloy:renderable alloy:renderer)))
              (alloy:bounds (&optional thing)
                (if thing
                    (alloy:bounds thing)
                    (alloy:extent-for alloy:renderable alloy:renderer))))
         (declare (ignorable #'alloy:focus #'alloy:bounds))
         (case shape
           ,@(loop for (name . initargs) in shapes
                   collect `(,name (merge-style-into (call-next-method)
                                                     (style ,@initargs ,@default))))
           (T (call-next-method)))))))

(defmethod alloy:register :after ((renderable renderable) (renderer renderer))
  (realize-renderable renderer renderable))

(defmethod alloy:render :before ((renderer renderer) (element alloy:layout))
  (simple:with-pushed-transforms (renderer)
    (simple:translate renderer (alloy:bounds element))
    (loop for shape across (shapes element)
          for style = (shape-style renderer (car shape) element)
          do (simple:with-pushed-transforms (renderer)
               (simple:with-pushed-styles (renderer)
                 (activate-style style renderer)
                 (alloy:render-with (cdr shape) element renderer))))))

(defmethod alloy:render-with ((renderer renderer) (element alloy:layout-element) (component alloy:component))
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

(defmethod shape-style ((renderer renderer) shape (renderable renderable))
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

(defmethod realize-renderable ((renderer renderer) (renderable renderable)))

(defmethod clear-shapes ((renderable renderable))
  (setf (fill-pointer (shapes renderable)) 0))

(defmethod find-shape (id (renderable renderable) &optional errorp)
  (or (cdr (find id (shapes renderable) :key #'car))
      (when errorp (error "No such shape~%  ~s~%in~%  ~s"
                          id renderable))))

(defmethod (setf find-shape) ((shape shape) id (renderable renderable))
  (alloy:mark-for-render renderable)
  (let ((record (find id (shapes renderable) :key #'car)))
    (if record
        (setf (cdr record) shape)
        (vector-push-extend (cons id shape) (shapes renderable)))
    shape))
