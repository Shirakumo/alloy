(in-package #:org.shirakumo.alloy.examples)

(defclass focus-tracker (alloy:popup alloy:renderable)
  ((tracked :initform NIL :accessor tracked)))

(presentations:define-realization (windowing:window focus-tracker)
  ((shape simple:rectangle)
   (alloy:margins -5)
   :line-width (alloy:un 2)
   :pattern (colored:color 0.5 0.8 1.0)))

(presentations:define-update (windowing:window focus-tracker)
  (shape :hidden-p (= 0 (alloy:pxw alloy:renderable))))

(presentations:define-animated-shapes focus-tracker
  (shape (simple:bounds :duration 0.5)))

(defmethod animation:update :before ((tracker focus-tracker) dt)
  (let ((focus (alloy:focused (alloy:ui tracker))))
    (unless (eq focus (tracked tracker))
      (setf (tracked tracker) focus)
      (if (typep focus 'alloy:layout-element)
          (alloy::with-global-bounds (bounds focus)
            (setf (alloy:bounds tracker) (alloy:px-extent
                                          (alloy:pxx bounds)
                                          (alloy:pxy bounds)
                                          (alloy:pxw bounds)
                                          (alloy:pxh bounds))))
          (setf (alloy:bounds tracker) (alloy:px-extent)))
      (alloy:mark-for-render tracker))))
