#|
 This file is a part of Alloy
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.alloy.renderers.simple.presentations)

(defclass renderer (simple:renderer)
  ())

(defclass shape ()
  ((style :initform (make-style) :accessor style)
   (style-override :initarg :style-override :initform (make-style) :accessor style-override)))

(defgeneric render-sized (renderer shape extent))

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

(defun make-style (&rest args)
  (apply #'make-instance 'style args))

(defgeneric merge-style-into (target source))
(defgeneric activate-style (style renderer))

(stealth-mixin:define-stealth-mixin renderable () alloy:renderable
  ;; Reminder for future me: this has to be a vector for insertion order to stay correct.
  ((shapes :initform (make-array 0 :adjustable T :fill-pointer T) :accessor shapes)))

(defgeneric override-shapes (renderable shapes))
(defgeneric override-style (renderable shape style))
(defgeneric realize-renderable (renderer renderable))
(defgeneric compute-shape-style (renderer shape renderable))
(defgeneric update-shape (renderer renderable shape))
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
    `(defmethod compute-shape-style ((alloy:renderer ,renderer) (shape symbol) (alloy:renderable ,renderable))
       (flet ((alloy:focus (&optional (thing alloy:renderable))
                (alloy:focus thing))
              (alloy:bounds (&optional (thing alloy:renderable))
                (alloy:bounds thing)))
         (declare (ignorable #'alloy:focus #'alloy:bounds))
         (case shape
           ,@(loop for (name . initargs) in shapes
                   collect `(,name (merge-style-into (call-next-method)
                                                     (make-style ,@initargs ,@default))))
           (T (call-next-method)))))))

(defmethod initialize-instance :around ((renderable renderable) &key style shapes)
  ;; Needs to be :AROUND to allow the subclass ALLOY:RENDERER to set the fields.
  (call-next-method)
  (when (and (not (slot-boundp renderable 'alloy:renderer)) (or style shapes))
    (arg! :renderer))
  (when shapes
    (override-shapes renderable shapes))
  (when style
    (loop for (shape . style) in style
          do (override-style renderable shape
                             (etypecase style
                               (style style)
                               (cons (apply #'make-style style)))))
    (update-shape (alloy:renderer renderable) renderable T)))

(defun make-default-style (renderer)
  (make-style :fill-color (simple:color 0 0 0)
              :line-width 1.0
              :font (simple:request-font renderer :default)
              :font-size 12.0
              :composite-mode :source-over
              :z-index 0
              :offset (alloy:point 0 0)
              :scale (alloy:size 1 1)
              :rotation 0.0
              :pivot (alloy:point 0 0)))

(defmethod alloy:register :around ((renderable renderable) (renderer renderer))
  ;; Needs to be :AROUND to allow the subclass ALLOY:RENDERER to set the fields.
  (call-next-method)
  (realize-renderable renderer renderable))

(defmethod alloy:render ((renderer renderer) (renderable renderable))
  (simple:with-pushed-transforms (renderer)
    (simple:translate renderer (alloy:bounds renderable))
    (loop for shape across (shapes renderable)
          do (simple:with-pushed-transforms (renderer)
               (simple:with-pushed-styles (renderer)
                 (render-sized renderer (cdr shape) (alloy:bounds renderable)))))))

(defmethod render-sized :before ((renderer renderer) (shape shape) size)
  (activate-style (style shape) renderer))

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
  (setf (simple:z-index renderer) (z-index style))
  ;; TODO: Not sure this is quite right.
  (simple:translate renderer (offset style))
  (simple:translate renderer (pivot style))
  (simple:rotate renderer (rotation style))
  (simple:scale renderer (scale style))
  (simple:translate renderer (alloy:point (- (alloy:point-x (pivot style)))
                                          (- (alloy:point-y (pivot style))))))

(defmethod compute-shape-style ((renderer renderer) shape (renderable renderable))
  (make-default-style renderer))

(defmethod compute-shape-style :around ((renderer renderer) (shape shape) (renderable renderable))
  (merge-style-into (call-next-method)
                    (style-override shape)))

(defmethod compute-shape-style ((renderer renderer) (shape shape) (renderable renderable))
  (compute-shape-style renderer (car (find shape (shapes renderable) :key #'cdr)) renderable))

(defmethod realize-renderable ((renderer renderer) (renderable renderable)))

(defmethod realize-renderable :after ((renderer renderer) (renderable renderable))
  (loop with renderer = (alloy:renderer renderable)
        for (name . shape) across (shapes renderable)
        do (setf (style shape) (compute-shape-style renderer name renderable))))

(defmethod override-style ((renderable renderable) (shape symbol) style)
  (override-style renderable (find-shape shape renderable T) style))

(defmethod override-style ((renderable renderable) (shape shape) (style style))
  (setf (style-override shape) style))

(defmethod override-style :after ((renderable renderable) (shape shape) (style style))
  (setf (style shape) (compute-shape-style (alloy:renderer renderable) shape renderable)))

(defmethod clear-shapes ((renderable renderable))
  (setf (fill-pointer (shapes renderable)) 0))

(defmethod find-shape (id (renderable renderable) &optional errorp)
  (or (cdr (find id (shapes renderable) :key #'car))
      (when errorp (error "No such shape~%  ~s~%in~%  ~s"
                          id renderable))))

(defmethod (setf find-shape) ((shape shape) id (renderable renderable))
  (let ((record (find id (shapes renderable) :key #'car)))
    (if record
        (setf (cdr record) shape)
        (vector-push-extend (cons id shape) (shapes renderable)))
    shape))

(defmethod (setf find-shape) ((null null) id (renderable renderable))
  (let ((pos (position id (shapes renderable) :key #'car)))
    (when pos
      (setf (shapes renderable) (array-utils:vector-pop-position (shapes renderable) pos)))))

(defmethod update-shape ((renderer renderer) (renderable renderable) (shape shape))
  (setf (style shape) (compute-shape-style renderer shape renderable)))

(defmethod update-shape ((renderer renderer) (renderable renderable) (name symbol))
  (update-shape renderer renderable (find-shape name renderable T)))

(defmethod update-shape ((renderer renderer) (renderable renderable) (all (eql T)))
  (loop for (name . shape) across (shapes renderable)
        do (update-shape renderer renderable name)))

(defmethod alloy:mark-for-render :after ((renderable renderable))
  (update-shape (alloy:renderer renderable) renderable T))
