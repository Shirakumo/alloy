#|
 This file is a part of Alloy
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.alloy.animation)

(defgeneric lerp (a b x))

(declaim (inline %lerp))
(defun %lerp (a b x)
  (declare (type single-float a b x))
  (declare (optimize speed))
  (+ (* a (- 1f0 x)) (* b x)))

(defmethod lerp ((a real) (b real) x)
  (%lerp (float a 0f0) (float b 0f0) (float x 0f0)))

(defmethod lerp ((a alloy:unit) (b alloy:unit) x)
  (alloy:px (%lerp (alloy:to-px a) (alloy:to-px b) (float x 0f0))))

(defmethod lerp ((a alloy:px) (b alloy:px) x)
  (alloy:px (%lerp (alloy:unit-value a) (alloy:unit-value b) (float x 0f0))))

(defmethod lerp ((a alloy:vw) (b alloy:vw) x)
  (alloy:vw (%lerp (alloy:unit-value a) (alloy:unit-value b) (float x 0f0))))

(defmethod lerp ((a alloy:vh) (b alloy:vh) x)
  (alloy:vh (%lerp (alloy:unit-value a) (alloy:unit-value b) (float x 0f0))))

(defmethod lerp ((a alloy:pw) (b alloy:pw) x)
  (alloy:pw (%lerp (alloy:unit-value a) (alloy:unit-value b) (float x 0f0))))

(defmethod lerp ((a alloy:ph) (b alloy:ph) x)
  (alloy:ph (%lerp (alloy:unit-value a) (alloy:unit-value b) (float x 0f0))))

(defmethod lerp ((a alloy:un) (b alloy:un) x)
  (alloy:un (%lerp (alloy:unit-value a) (alloy:unit-value b) (float x 0f0))))

(defmethod lerp ((a alloy:cm) (b alloy:cm) x)
  (alloy:cm (%lerp (alloy:unit-value a) (alloy:unit-value b) (float x 0f0))))

(defmethod lerp ((a alloy:point) (b alloy:point) x)
  (alloy:point (lerp (alloy:point-x a) (alloy:point-x b) x)
               (lerp (alloy:point-y a) (alloy:point-y b) x)))

(defmethod lerp ((a alloy:size) (b alloy:size) x)
  (alloy:size (lerp (alloy:size-w a) (alloy:size-w b) x)
              (lerp (alloy:size-h a) (alloy:size-h b) x)))

(defmethod lerp ((a alloy:margins) (b alloy:margins) x)
  (alloy:margins (lerp (alloy:margins-l a) (alloy:margins-l b) x)
                 (lerp (alloy:margins-u a) (alloy:margins-u b) x)
                 (lerp (alloy:margins-r a) (alloy:margins-r b) x)
                 (lerp (alloy:margins-b a) (alloy:margins-b b) x)))

(defmethod lerp ((a alloy:extent) (b alloy:extent) x)
  (alloy:extent (lerp (alloy:extent-x a) (alloy:extent-x b) x)
                (lerp (alloy:extent-y a) (alloy:extent-y b) x)
                (lerp (alloy:extent-w a) (alloy:extent-w b) x)
                (lerp (alloy:extent-h a) (alloy:extent-h b) x)))

(defmethod lerp ((a colored:rgb) (b colored:rgb) x)
  (let ((x (float x 0f0)))
    (colored:rgb (%lerp (colored:r a) (colored:r b) x)
                 (%lerp (colored:g a) (colored:g b) x)
                 (%lerp (colored:b a) (colored:b b) x)
                 (%lerp (colored:a a) (colored:a b) x))))

(defmethod lerp ((a colored:hsv) (b colored:hsv) x)
  (let ((x (float x 0f0)))
    (colored:hsv (%lerp (colored:h a) (colored:h b) x)
                 (%lerp (colored:s a) (colored:s b) x)
                 (%lerp (colored:v a) (colored:v b) x)
                 (%lerp (colored:a a) (colored:a b) x))))

(defmethod lerp ((a colored:hsl) (b colored:hsl) x)
  (let ((x (float x 0f0)))
    (colored:hsl (%lerp (colored:h a) (colored:h b) x)
                 (%lerp (colored:s a) (colored:s b) x)
                 (%lerp (colored:l a) (colored:l b) x)
                 (%lerp (colored:a a) (colored:a b) x))))

(defmethod lerp ((a colored:hsi) (b colored:hsi) x)
  (let ((x (float x 0f0)))
    (colored:hsi (%lerp (colored:h a) (colored:h b) x)
                 (%lerp (colored:s a) (colored:s b) x)
                 (%lerp (colored:i a) (colored:i b) x)
                 (%lerp (colored:a a) (colored:a b) x))))

(defmethod lerp ((a colored:cmyk) (b colored:cmyk) x)
  (let ((x (float x 0f0)))
    (colored:cmyk (%lerp (colored:c a) (colored:c b) x)
                  (%lerp (colored:m a) (colored:m b) x)
                  (%lerp (colored:y a) (colored:y b) x)
                  (%lerp (colored:k a) (colored:k b) x)
                  (%lerp (colored:a a) (colored:a b) x))))

#++
(defmethod lerp ((a colored:xyz) (b colored:xyz) x)
  (let ((x (float x 0f0)))
    (colored:xyz (%lerp (colored:x* a) (colored:x* b) x)
                 (%lerp (colored:y* a) (colored:y* b) x)
                 (%lerp (colored:z* a) (colored:z* b) x)
                 (%lerp (colored:a a) (colored:a b) x))))

#++
(defmethod lerp ((a colored:lab) (b colored:lab) x)
  (let ((x (float x 0f0)))
    (colored:lab (%lerp (colored:l* a) (colored:l* b) x)
                 (%lerp (colored:a* a) (colored:a* b) x)
                 (%lerp (colored:b* a) (colored:b* b) x)
                 (%lerp (colored:a a) (colored:a b) x))))
