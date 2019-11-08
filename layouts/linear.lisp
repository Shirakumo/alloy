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
   (cell-margins :initarg :cell-margins :initform (margins) :accessor cell-margins)))

(defgeneric update-linear-layout (layout index extent))

(defmethod (setf align) :after (value (layout linear-layout))
  (update-linear-layout layout 0 (bounds layout))
  (notice-bounds layout (layout-parent layout)))

(defmethod (setf stretch) :after (value (layout linear-layout))
  (update-linear-layout layout 0 (bounds layout))
  (notice-bounds layout (layout-parent layout)))

(defmethod (setf min-size) :after (value (layout linear-layout))
  (update-linear-layout layout 0 (bounds layout))
  (notice-bounds layout (layout-parent layout)))

(defmethod notice-bounds ((element layout-element) (layout linear-layout))
  ;; TODO: optimise bounds update.
  (let ((updated (update-linear-layout layout 0 (bounds layout))))
    (unless (extent= (bounds layout) updated)
      (setf (bounds layout) updated)
      (notice-bounds layout (layout-parent layout)))))

(defmethod (setf bounds) :after (extent (layout linear-layout))
  (update-linear-layout layout 0 extent))

(defclass vertical-linear-layout (linear-layout)
  ())

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
          (do-elements (element layout :start 0 :result (px-extent x oy w (- y oy)))
            (destructure-extent (:w ew :h eh :to-px T) (suggest-bounds (px-extent x b mw mh) element)
              (setf w (max w (+ ew l r)))
              (ecase (align layout)
                (:start (incf y (+ eh u b)))
                (:end (decf y (+ eh u b)))))))))))

(defmethod update-linear-layout ((layout vertical-linear-layout) index extent)
  (with-unit-parent layout
    (destructure-margins (:l l :u u :r r :b b :to-px T) (cell-margins layout)
      (destructure-extent (:x x :y oy :w w :h h :to-px T) extent
        (let ((mh (pxh (min-size layout)))
              (mw (if (stretch layout) (- w l r) (pxw (min-size layout))))
              (y (ecase (align layout)
                   (:start (+ oy b))
                   (:end (- (+ oy h) u))))
              (x (+ l x)))
          (do-elements (element layout :start index :result (px-extent x oy w (- y oy)))
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

(defmethod update-linear-layout ((layout horizontal-linear-layout) index extent)
  (with-unit-parent layout
    (destructure-margins (:l l :u u :r r :b b :to-px T) (cell-margins layout)
      (destructure-extent (:x ox :y y :w w :h h :to-px T) extent
        (let ((mw (pxw (min-size layout)))
              (mh (if (stretch layout) (- h u b) (pxh (min-size layout))))
              (x (ecase (align layout)
                   (:start (+ ox l))
                   (:end (- (+ ox w) r))))
              (y (+ y b)))
          (do-elements (element layout :start index :result (px-extent ox y (- x ox) h))
            (destructure-extent (:w ew :h eh :to-px T) (suggest-bounds (px-extent x y mw mh) element)
              (setf (bounds element)
                    (px-extent (ecase (align layout)
                                 (:start (prog1 x (incf x (+ ew l r))))
                                 (:end (decf x (+ ew l r))))
                               y
                               ew
                               (if (stretch layout) (- h u b) (min (- h u b) eh)))))))))))
