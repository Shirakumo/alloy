#|
 This file is a part of Alloy
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.alloy.animation)

(defgeneric state-properties (element part observable value))
(defgeneric update-state (element observable value))
(defgeneric map-parts (function element))

(defstruct (property (:constructor make-property (place value &key (priority 0) (duration 1f0) (easing 'linear))))
  (place NIL :type symbol)
  (value NIL :type T)
  (priority 0 :type (signed-byte 32))
  (duration 1f0 :type single-float)
  (easing (easing 'linear) :type function))

(defun compile-tweens (properties animated)
  (let ((props ()))
    ;; de-duplicate and only keep top priority per setter
    (loop for property in properties
          for setter = (property-setter property)
          for priority = (property-priority property)
          do (loop for cons on props
                   for prop = (car cons)
                   do (when (and (eql (property-setter prop) setter)
                                 (< (property-priority prop) priority))
                        (setf (car cons) property)
                        (return))
                   finally (push property props)))
    ;; turn the props into tweens
    (let ((tweens (make-array (length props))))
      (dotimes (i (length tweens) tweens)
        (let* ((property (pop props))
               (stops (make-array 2 :element-type 'single-float))
               (values (make-array 2))
               (place (property-place property)))
          (setf (aref stops 0) 0f0)
          (setf (aref stops 1) (property-duration property))
          (setf (aref values 0) (funcall place animated))
          (setf (aref values 1) (property-value property))
          (setf (aref tweens i)
                (make-tween (fdefinition `(setf ,place))
                            stops
                            values
                            (make-array 1 :initial-element (property-easing property)))))))))

(defmacro define-state ((class observable value) &body body)
  (let ((part (gensym "PART")))
    `(defmethod state-properties ((alloy:renderable ,class) ,part (alloy:observable (eql ,observable)) (alloy:value (eql ,value)))
       (case ,part
         ,@(loop for (part . properties) in body
                 collect `(,part
                           (list ,@(loop for property in properties
                                         collect `(make-property ,@property)))))))))

(defclass renderable (alloy:renderable alloy:observable)
  ())

(defun update-part (renderable part observable value)
  (let ((properties (state-properties element observable value)))
    (when properties
      (reinitialize-instance part :tweens (compile-tweens properties part)))))

(defmethod update-state ((renderable renderable) observable value)
  (alloy:do-elements (part renderable)
    (update-part renderable part observable value)))

(defmethod alloy:notify-observers :after ((renderable renderable) observable &rest args)
  (update-state renderable observable (first args)))
