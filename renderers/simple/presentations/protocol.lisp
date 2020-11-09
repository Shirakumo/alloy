#|
 This file is a part of Alloy
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.alloy.renderers.simple.presentations)

(defclass renderer (simple:renderer)
  ())

;;; TODO: The shapes should be able to influence the renderable's "ideal size" as returned
;;;       by the SUGGEST-BOUNDS function. This is a bit tricky, since some shapes might have
;;;       bounds that are just adaptive, whereas others might have bounds that are restrictive
;;;       and we need to only consider restrictive bounds.

;;; KLUDGE: This is "poisoning" as it injects a class that might not be wanted if other
;;;         systems than presentations are used to perform drawing operations. Ideally
;;;         we'd instead create subclasses of all shapes with our extra stuff as a mixin.
;;;         However, that's a bit troublesome with user-extensions, as those would not
;;;         automatically be presentations-ready.
;;;         
;;;         The same problem exists for renderables, though there it is arguably worse,
;;;         as then the user would always have to use the extra subclasses from presentations
;;;         to construct all their UI, rather than the standard ones from core.
(stealth-mixin:define-stealth-mixin shape () simple:shape
  ((name :initarg :name :initform NIL :reader name)
   (composite-mode :initarg :composite-mode :initform :source-over :accessor composite-mode)
   (z-index :initarg :z-index :initform 0 :accessor z-index)
   (offset :initarg :offset :initform (alloy:px-point 0 0) :accessor offset)
   (scale :initarg :scale :initform (alloy:px-size 1 1) :accessor scale)
   (rotation :initarg :rotation :initform 0f0 :accessor rotation)
   (pivot :initarg :pivot :initform (alloy:px-point 0 0) :accessor pivot)
   (hidden-p :initarg :hidden-p :initform NIL :accessor hidden-p)))

(stealth-mixin:define-stealth-mixin renderable (animation:animated) alloy:renderable
  ;; Reminder for future me: this has to be a vector for insertion order to stay correct.
  ((shapes :initform (make-array 0 :adjustable T :fill-pointer T) :accessor shapes)
   (update-overrides :initform () :accessor update-overrides)))

(defgeneric realize-renderable (renderer renderable))
(defgeneric update-shape (renderer renderable shape)
  (:method-combination progn :most-specific-last))
(defgeneric clear-shapes (renderable))
(defgeneric find-shape (id renderable &optional errorp))
(defgeneric (setf find-shape) (shape id renderable))

(defmacro define-realization ((renderer renderable &optional append) &body shapes)
  `(defmethod realize-renderable ((alloy:renderer ,renderer) (alloy:renderable ,renderable))
     ,(if append
          `(call-next-method)
          `(clear-shapes alloy:renderable))
     (symbol-macrolet ((alloy:focus (alloy:focus alloy:renderable))
                       (alloy:bounds (alloy:bounds alloy:renderable))
                       (alloy:value (alloy:value alloy:renderable))
                       (alloy:text (alloy:text alloy:renderable)))
       (declare (ignorable alloy:focus alloy:bounds alloy:value))
       ,@(loop for shape in shapes
               collect (destructuring-bind ((name type) &body initargs) shape
                         `(setf (find-shape ',name alloy:renderable)
                                (,type alloy:renderer ,@initargs :name ',name)))))
     alloy:renderable))

(defmacro define-update ((renderer renderable) &body shapes)
  (let* ((default (find T shapes :key #'car))
         (shapes (if default (remove default shapes) shapes)))
    `(defmethod update-shape progn ((alloy:renderer ,renderer) (alloy:renderable ,renderable) (shape shape))
       (symbol-macrolet ((alloy:focus (alloy:focus alloy:renderable))
                         (alloy:bounds (alloy:bounds alloy:renderable))
                         (alloy:value (alloy:value alloy:renderable))
                         (alloy:text (alloy:text alloy:renderable)))
         (declare (ignorable alloy:focus alloy:bounds alloy:value))
         (case (name shape)
           ,@(loop for (name . initargs) in shapes
                   collect `(,name (reinitialize-instance shape ,@initargs))))))))

(defmethod initialize-instance :after ((renderable renderable) &key style shapes)
  (dolist (shape shapes)
    (setf (find-shape (name shape) renderable) shape))
  (setf (update-overrides renderable) style))

(defmethod alloy:register :around ((renderable renderable) (renderer renderer))
  ;; Needs to be :AROUND to allow the subclass ALLOY:RENDERER to set the fields.
  (call-next-method)
  (when (= 0 (length (shapes renderable)))
    (realize-renderable renderer renderable)))

(defmethod alloy:render ((renderer renderer) (renderable renderable))
  (simple:with-pushed-transforms (renderer)
    (simple:translate renderer (alloy:bounds renderable))
    (loop for (name . shape) across (shapes renderable)
          unless (hidden-p shape)
          do (simple:with-pushed-transforms (renderer)
               (setf (simple:composite-mode renderer) (composite-mode shape))
               (incf (simple:z-index renderer) (z-index shape))
               ;; TODO: Not sure this is quite right.
               (simple:translate renderer (offset shape))
               (simple:translate renderer (pivot shape))
               (simple:rotate renderer (rotation shape))
               (simple:scale renderer (scale shape))
               (simple:translate renderer (alloy:px-point (- (alloy:pxx (pivot shape)))
                                                          (- (alloy:pxy (pivot shape)))))
               (alloy:render renderer shape))))
  (when (next-method-p) (call-next-method)))

(defmethod realize-renderable ((renderer renderer) (renderable renderable)))

(defmethod realize-renderable :around ((renderer renderer) (renderable renderable))
  (alloy:with-unit-parent renderable
    (call-next-method)))

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

(defmethod (setf update-overrides) :after (overrides (renderable renderable))
  (alloy:mark-for-render renderable))

(defmethod update-shape :around ((renderer renderer) (renderable renderable) (shape shape))
  (call-next-method)
  (let ((initargs (cdr (assoc (name shape) (update-overrides renderable)))))
    (when initargs ;; FIXME: What if the initargs should be dynamic?
      (apply #'reinitialize-instance shape initargs))))

(defmethod update-shape progn ((renderer renderer) (renderable renderable) (all (eql T)))
  (loop for (name . shape) across (shapes renderable)
        do (update-shape renderer renderable shape)))

;;; Defer updating the shapes until we have a dirty render.
;;; Might cause hiccups during render if there's many updates, but we save
;;; on tons of batch updates that would never be shown.
(defmethod alloy:render :before ((renderer renderer) (renderable renderable))
  (when (alloy:render-needed-p renderable)
    (when (= 0 (array-total-size (shapes renderable)))
      (realize-renderable renderer renderable))
    (update-shape renderer renderable T)))

(defmethod animation:map-parts (func (renderable renderable))
  (loop for shape across (shapes renderable)
        do (funcall func shape)))
