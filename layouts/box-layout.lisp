#|
 This file is a part of Alloy
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.alloy)

(defclass box-layout (layout)
  ((min-size :initarg :min-size :initform (size) :accessor min-size)
   (stretch :initarg :stretch :initform T :accessor stretch)))

(defgeneric update-box-layout (layout index extent ui))

(defmethod notice-bounds :after ((element layout-element) (layout box-layout))
  ;; FIXME: How do we get access to the UI instance?
  (let ((updated (update-box-layout layout (element-index element layout) (bounds layout) ui)))
    (unless (extent= (bounds layout) updated)
      (setf (bounds layout) updated))))

(defmethod suggest-bounds (extent (layout box-layout) ui)
  (update-box-layout layout 0 extent ui))

(defclass vertical-box-layout (box-layout)
  ())

(defmethod update-box-layout ((layout vertical-box-layout) index extent ui)
  (destructure-extent (:x x :y oy :w w) extent
    (let ((mh (size-h (min-size layout)))
          (mw (if (stretch layout) w (size-w (min-size layout))))
          (y oy))
      (do-elements (element layout :start index :result (extent x oy w (- y oy)))
        (let ((ideal (suggest-bounds (extent x y mw mh) element ui))
              (bounds (bounds element)))
          (setf (extent-x bounds) x)
          (setf (extent-y bounds) y)
          (setf (extent-w bounds) (if (stretch layout) w (extent-w ideal)))
          (setf (extent-h bounds) (extent-h ideal))
          (incf y (extent-h bounds)))))))

(defclass horizontal-box-layout (box-layout)
  ())

(defmethod update-box-layout ((layout horizontal-box-layout) index extent ui)
  (destructure-extent (:x ox :y y :h h) extent
    (let ((mh (if (stretch layout) h (size-h (min-size layout))))
          (mw (size-w (min-size layout)))
          (x ox))
      (do-elements (element layout :start index :result (extent ox y (- x ox) h))
        (let ((ideal (suggest-bounds (extent x y mw mh) element ui))
              (bounds (bounds element)))
          (setf (extent-x bounds) x)
          (setf (extent-y bounds) y)
          (setf (extent-w bounds) (extent-w ideal))
          (setf (extent-h bounds) (if (stretch layout) h (extent-h ideal)))
          (incf x (extent-w bounds)))))))
