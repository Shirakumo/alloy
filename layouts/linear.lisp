#|
 This file is a part of Alloy
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.alloy)

(defclass linear-layout (layout)
  ((min-size :initarg :min-size :initform (size) :accessor min-size)
   (stretch :initarg :stretch :initform T :accessor stretch)
   (align :initarg :align :initform :start :accessor align)))

(defgeneric update-linear-layout (layout index extent))

(defmethod (setf align) :after (value (layout linear-layout))
  (update-linear-layout layout 0 (bounds layout)))

(defmethod (setf stretch) :after (value (layout linear-layout))
  (update-linear-layout layout 0 (bounds layout)))

(defmethod (setf min-size) :after (value (layout linear-layout))
  (update-linear-layout layout 0 (bounds layout)))

(defmethod notice-bounds :after ((element layout-element) (layout linear-layout))
  (let ((updated (update-linear-layout layout (element-index element layout) (bounds layout))))
    (unless (extent= (bounds layout) updated)
      (setf (bounds layout) updated))))

(defmethod suggest-bounds (extent (layout linear-layout))
  (update-linear-layout layout 0 extent))

(defclass vertical-linear-layout (linear-layout)
  ())

(defmethod update-linear-layout ((layout vertical-linear-layout) index extent)
  (destructure-extent (:x x :y oy :w w :h h) extent
    (let ((mh (size-h (min-size layout)))
          (mw (if (stretch layout) w (size-w (min-size layout))))
          (y (ecase (align layout)
               (:start oy) (:end (+ oy h)))))
      (do-elements (element layout :start index :result (extent x oy w (- y oy)))
        (let ((ideal (suggest-bounds (extent x y mw mh) element))
              (bounds (bounds element)))
          (setf (extent-x bounds) x)
          (setf (extent-w bounds) (if (stretch layout) w (extent-w ideal)))
          (setf (extent-h bounds) (extent-h ideal))
          (ecase (align layout)
            (:start
             (setf (extent-y bounds) y)
             (incf y (extent-h bounds)))
            (:end
             (decf y (extent-h bounds))
             (setf (extent-y bounds) y))))))))

(defclass horizontal-linear-layout (linear-layout)
  ())

(defmethod update-linear-layout ((layout horizontal-linear-layout) index extent)
  (destructure-extent (:x ox :y y :w w :h h) extent
    (let ((mh (if (stretch layout) h (size-h (min-size layout))))
          (mw (size-w (min-size layout)))
          (x (ecase (align layout)
               (:start ox) (:end (+ ox w)))))
      (do-elements (element layout :start index :result (extent ox y (- x ox) h))
        (let ((ideal (suggest-bounds (extent x y mw mh) element))
              (bounds (bounds element)))
          (setf (extent-y bounds) y)
          (setf (extent-w bounds) (extent-w ideal))
          (setf (extent-h bounds) (if (stretch layout) h (extent-h ideal)))
          (ecase (align layout)
            (:start
             (setf (extent-x bounds) x)
             (incf x (extent-w bounds)))
            (:end
             (decf x (extent-w bounds))
             (setf (extent-x bounds) x))))))))
