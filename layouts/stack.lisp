#|
 This file is a part of Alloy
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.alloy)

(defclass stack-layout (layout vector-container)
  ((index :initform 0 :accessor index)))

(defmethod current ((layout stack-layout))
  (when (< 0 (length (elements layout)))
    (aref (elements layout) (index layout))))

(defmethod (setf current) :before ((current layout-element) (layout stack-layout))
  (unless (eq layout (layout-parent current))
    (error 'element-has-different-parent
           :parent (layout-parent current) :element current :container layout)))

(defmethod (setf current) ((current layout-element) (layout stack-layout))
  (setf (index layout) (element-index current layout)))

(defmethod (setf index) :before (index (layout stack-layout))
  (unless (<= 0 index (1- (length (elements layout))))
    (error 'index-out-of-range
           :index index :range (list 0 (length (elements layout))))))

(defmethod (setf index) :after (index (layout stack-layout))
  (setf (bounds (current layout)) (bounds layout)))

(defmethod leave :after ((element layout-element) (layout stack-layout))
  (when (and (< 0 (index layout))
             (<= (length (elements layout)) (index layout)))
    (decf (index layout))))

(defmethod notice-bounds ((element layout-element) (layout stack-layout))
  (notice-bounds element (layout-parent layout)))

(defmethod suggest-bounds (extent (layout stack-layout))
  (if (= 0 (length (elements layout)))
      extent
      (suggest-bounds extent (current layout))))

(defmethod (setf bounds) :after (extent (layout stack-layout))
  (when (< 0 (length (elements layout)))
    (setf (bounds (current layout)) extent)))

(defmethod render ((renderer renderer) (layout stack-layout))
  (when (< 0 (length (elements layout)))
    (render renderer (current layout))))

(defmethod maybe-render ((renderer renderer) (layout stack-layout))
  (when (< 0 (length (elements layout)))
    (maybe-render renderer (current layout))))

(defmethod ensure-visible ((element layout-element) (layout stack-layout))
  (loop until (or (eq layout (layout-parent element))
                  (eq element (layout-parent element)))
        do (setf element (layout-parent element)))
  (let ((index (position element (elements layout))))
    (when index (setf (index layout) index)))
  (call-next-method))
