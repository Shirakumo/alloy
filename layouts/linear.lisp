#|
 This file is a part of Alloy
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.alloy)

(defclass linear-layout (layout vector-container)
  ((min-size :initarg :min-size :initform (size 20 20) :accessor min-size)
   (stretch :initarg :stretch :initform T :accessor stretch)
   (align :initarg :align :initform :start :accessor align)
   (cell-margins :initarg :cell-margins :initform (margins 2) :accessor cell-margins)))

(defgeneric fit-linear-layout-contents (layout extent))

(defmethod update-linear-layout ((layout linear-layout) extent)
  (let ((updated (fit-linear-layout-contents layout extent)))
    (unless (extent= (bounds layout) updated)
      (setf (bounds layout) updated)
      (when (slot-boundp layout 'layout-parent)
        (notice-bounds layout (layout-parent layout))))))

(defmethod (setf align) :after (value (layout linear-layout))
  (update-linear-layout layout (bounds layout)))

(defmethod (setf stretch) :after (value (layout linear-layout))
  (update-linear-layout layout (bounds layout)))

(defmethod (setf min-size) :after (value (layout linear-layout))
  (update-linear-layout layout (bounds layout)))

(defmethod notice-bounds ((element layout-element) (layout linear-layout))
  (update-linear-layout layout (bounds layout)))

(defmethod (setf bounds) :after (extent (layout linear-layout))
  (fit-linear-layout-contents layout extent))

(defclass vertical-linear-layout (linear-layout)
  ((align :initform :end)))

(defmethod suggest-bounds (extent (layout vertical-linear-layout))
  (with-unit-parent layout
    (destructure-margins (:l l :u u :r r :b b :to-px T) (cell-margins layout)
      (destructure-extent (:x x :y oy :w w :h h :to-px T) extent
        (let ((mh (pxh (min-size layout)))
              (mw (if (stretch layout) (- w l r) (pxw (min-size layout))))
              (y (ecase (align layout)
                   (:start (+ oy b))
                   (:end (- (+ oy h) u))))
              (x (+ l x)))
          (do-elements (element layout :result (px-extent x oy w (- (+ oy h) y)))
            (destructure-extent (:w ew :h eh :to-px T) (suggest-bounds (px-extent x b mw mh) element)
              (setf w (max w (+ ew l r)))
              (ecase (align layout)
                (:start (incf y (+ eh u b)))
                (:end (decf y (+ eh u b)))))))))))

(defmethod fit-linear-layout-contents ((layout vertical-linear-layout) extent)
  (with-unit-parent layout
    (destructure-margins (:l l :u u :r r :b b :to-px T) (cell-margins layout)
      (destructure-extent (:x x :y oy :w w :h h :to-px T) extent
        (let ((mh (pxh (min-size layout)))
              (mw (if (stretch layout) (- w l r) (pxw (min-size layout))))
              (y (ecase (align layout)
                   (:start (+ oy b))
                   (:end (- (+ oy h) u))))
              (x (+ l x)))
          (do-elements (element layout :result (px-extent x oy w (- oy y)))
            (destructure-extent (:w ew :h eh :to-px T) (suggest-bounds (px-extent x y mw mh) element)
              (setf (bounds element)
                    (px-extent x
                               (ecase (align layout)
                                 (:start (prog1 y (incf y (+ eh u b))))
                                 (:end (decf y (+ eh u b))))
                               (if (stretch layout) (- w l r) (min (- w l r) ew))
                               eh)))))))))

(defclass horizontal-linear-layout (linear-layout)
  ())

(defmethod suggest-bounds (extent (layout horizontal-linear-layout))
  (with-unit-parent layout
    (destructure-margins (:l l :u u :r r :b b :to-px T) (cell-margins layout)
      (destructure-extent (:x ox :y y :w w :h h :to-px T) extent
        (let ((mw (pxw (min-size layout)))
              (mh (if (stretch layout) (- h u b) (pxh (min-size layout))))
              (x (ecase (align layout)
                   (:start (+ ox l))
                   (:end (- (+ ox w) r))))
              (y (+ y b)))
          (do-elements (element layout :start 0 :result (px-extent ox y (- x ox) h))
            (destructure-extent (:w ew :h eh :to-px T) (suggest-bounds (px-extent x b mw mh) element)
              (setf h (max h (+ eh u b)))
              (ecase (align layout)
                (:start (incf x (+ ew l r)))
                (:end (decf x (+ ew l r)))))))))))

(defmethod fit-linear-layout-contents ((layout horizontal-linear-layout) extent)
  (with-unit-parent layout
    (destructure-margins (:l l :u u :r r :b b :to-px T) (cell-margins layout)
      (destructure-extent (:x ox :y y :w w :h h :to-px T) extent
        (let ((mw (pxw (min-size layout)))
              (mh (if (stretch layout) (- h u b) (pxh (min-size layout))))
              (x (ecase (align layout)
                   (:start (+ ox l))
                   (:end (- (+ ox w) r))))
              (y (+ y b)))
          (do-elements (element layout :result (px-extent ox y (- x ox) h))
            (destructure-extent (:w ew :h eh :to-px T) (suggest-bounds (px-extent x y mw mh) element)
              (setf (bounds element)
                    (px-extent (ecase (align layout)
                                 (:start (prog1 x (incf x (+ ew l r))))
                                 (:end (decf x (+ ew l r))))
                               y
                               ew
                               (if (stretch layout) (- h u b) (min (- h u b) eh)))))))))))
