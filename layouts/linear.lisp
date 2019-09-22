#|
 This file is a part of Alloy
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.alloy)

(defclass linear-layout (layout vector-container)
  ((min-size :initarg :min-size :initform (size 20 20) :accessor min-size)
   (stretch :initarg :stretch :initform T :accessor stretch)
   (align :initarg :align :initform :start :accessor align)))
;; FIXME: cell-margins

(defgeneric update-linear-layout (layout index extent))

(defmethod (setf align) :after (value (layout linear-layout))
  (update-linear-layout layout 0 (bounds layout)))

(defmethod (setf stretch) :after (value (layout linear-layout))
  (update-linear-layout layout 0 (bounds layout)))

(defmethod (setf min-size) :after (value (layout linear-layout))
  (update-linear-layout layout 0 (bounds layout)))

(defmethod notice-bounds ((element layout-element) (layout linear-layout))
  ;; FIXME: optimise bounds update.
  (let ((updated (update-linear-layout layout 0 (bounds layout))))
    (unless (extent= (bounds layout) updated)
      (setf (bounds layout) updated))))

(defmethod (setf bounds) :after (extent (layout linear-layout))
  (update-linear-layout layout 0 extent))

(defclass vertical-linear-layout (linear-layout)
  ())

(defmethod suggest-bounds (extent (layout vertical-linear-layout))
  (destructure-extent (:x x :y oy :w w :h h) extent
    (let ((mh (size-h (min-size layout)))
          (mw (if (stretch layout) w (size-w (min-size layout))))
          (y (ecase (align layout)
               (:start oy) (:end (+ oy h)))))
      (do-elements (element layout :start 0 :result (extent x oy w (- y oy)))
        (let ((ideal (suggest-bounds (extent x y mw mh) element)))
          (setf w (max w (extent-w ideal)))
          (ecase (align layout)
            (:start
             (incf y (extent-h ideal)))
            (:end
             (decf y (extent-h ideal)))))))))

(defmethod update-linear-layout ((layout vertical-linear-layout) index extent)
  (destructure-extent (:x x :y oy :w w :h h) extent
    (let ((mh (size-h (min-size layout)))
          (mw (if (stretch layout) w (size-w (min-size layout))))
          (y (ecase (align layout)
               (:start oy) (:end (+ oy h)))))
      (do-elements (element layout :start index :result (extent x oy w (- y oy)))
        (let ((ideal (suggest-bounds (extent x y mw mh) element)))
          (setf (bounds element)
                (extent x
                        (ecase (align layout)
                          (:start
                           (prog1 y
                             (incf y (extent-h ideal))))
                          (:end
                           (decf y (extent-h ideal))
                           y))
                        (if (stretch layout) w (extent-w ideal))
                        (extent-h ideal))))))))

(defclass horizontal-linear-layout (linear-layout)
  ())

(defmethod suggest-bounds (extent (layout horizontal-linear-layout))
  (destructure-extent (:x ox :y y :w w :h h) extent
    (let ((mh (if (stretch layout) h (size-h (min-size layout))))
          (mw (size-w (min-size layout)))
          (x (ecase (align layout)
               (:start ox) (:end (+ ox w)))))
      (do-elements (element layout :start 0 :result (extent ox y (- x ox) h))
        (let ((ideal (suggest-bounds (extent x y mw mh) element)))
          (setf h (max h (extent-h ideal)))
          (ecase (align layout)
            (:start
             (incf x (extent-w ideal)))
            (:end
             (setf (extent-x ideal) x))))))))

(defmethod update-linear-layout ((layout horizontal-linear-layout) index extent)
  (destructure-extent (:x ox :y y :w w :h h) extent
    (let ((mh (if (stretch layout) h (size-h (min-size layout))))
          (mw (size-w (min-size layout)))
          (x (ecase (align layout)
               (:start ox) (:end (+ ox w)))))
      (do-elements (element layout :start index :result (extent ox y (- x ox) h))
        (let ((ideal (suggest-bounds (extent x y mw mh) element)))
          (setf (bounds element)
                (extent (ecase (align layout)
                          (:start
                           (prog1 x
                             (incf x (extent-w ideal))))
                          (:end
                           (decf x (extent-w ideal))
                           x))
                        y
                        (extent-w ideal)
                        (if (stretch layout) h (extent-h ideal)))))))))
