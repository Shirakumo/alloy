#|
 This file is a part of Alloy
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.alloy)

(defclass popup (layout single-container)
  ())

(defmethod location ((layout popup))
  (bounds layout))

(defmethod (setf location) (location (layout popup))
  (setf (slot-value layout 'bounds)
        (px-extent (pxx location) (pxy location) (w (bounds layout)) (h (bounds layout)))))

;;; Ignore bounds enforced by the outside
(defmethod (setf bounds) (bounds (layout popup)))

(defmethod suggest-bounds (extent (layout popup))
  extent)

;;; Let inner element govern size.
(defmethod notice-bounds ((element layout-element) (layout popup))
  (setf (slot-value layout 'bounds)
        (px-extent (x (bounds layout)) (y (bounds layout)) (pxw (bounds element)) (pxh (bounds element)))))

(defmethod render ((renderer renderer) (layout popup))
  (when (inner layout)
    (render renderer (inner layout))))
