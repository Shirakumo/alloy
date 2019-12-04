#|
 This file is a part of Alloy
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.alloy)

(defclass flow-layout (layout vector-container)
  ((cell-margins :initarg :cell-margins :initform (margins 5) :accessor cell-margins)
   (align :initarg :align :initform :end :accessor align)
   (min-size :initarg :min-size :initform (size 20 20) :accessor min-size)))

(defmethod notice-bounds ((element layout-element) (layout flow-layout))
  (if (eq layout (layout-parent layout))
      (setf (bounds layout) (bounds layout))
      (let ((updated (suggest-bounds (bounds layout) layout)))
        (unless (extent= (bounds layout) updated)
          (setf (bounds layout) updated)))))

(defmethod suggest-bounds (extent (layout flow-layout))
  (with-unit-parent layout
    (destructure-margins (:l ml :u mu :r mr :b mb :to-px T) (cell-margins layout)
      (destructure-extent (:x ox :y oy :w w :h oh :to-px T) extent
        (let* ((mh (pxh (min-size layout)))
               (mw (pxw (min-size layout)))
               (y (ecase (align layout)
                    (:start (+ oy mb))
                    (:end (- (+ oy oh) mu))))
               (x (+ ml ox))
               (rh mh)
               (elements (elements layout))
               (i 0))
          (loop while (< i (length elements))
                for element = (aref elements i)
                do (destructure-extent (:w ew :h eh :to-px T) (suggest-bounds (px-extent x y mw rh) element)
                     (setf rh (max rh eh))
                     (cond ((<= w (+ x ew ml mr))
                            ;; Row overflow, flush row and retry.
                            (unless (= x (+ ml ox))
                              ;; Only retry if the element does not consume the entire row anyway.
                              (decf i))
                            (ecase (align layout)
                              (:start (incf y (+ rh mu mb)))
                              (:end (decf y (+ rh mu mb))))
                            (setf x (+ ml ox))
                            (setf rh mh))
                           (T
                            (incf x (+ ew ml mr)))))
                   (incf i))
          (px-extent ox oy w (ecase (align layout)
                               (:start (- (+ y mu) oy))
                               (:end (- (+ oy oh) y mb)))))))))

(defmethod (setf bounds) :after (extent (layout flow-layout))
  (with-unit-parent layout
    (destructure-margins (:l ml :u mu :r mr :b mb :to-px T) (cell-margins layout)
      (destructure-extent (:x ox :y oy :w w :h h :to-px T) extent
        (let* ((mh (pxh (min-size layout)))
               (mw (pxw (min-size layout)))
               (y (ecase (align layout)
                    (:start (+ oy mb))
                    (:end (- (+ oy h) mu))))
               (x (+ ml ox))
               (rh mh)
               (elements (elements layout))
               (i 0))
          (loop while (< i (length elements))
                for element = (aref elements i)
                do (destructure-extent (:w ew :h eh :to-px T) (suggest-bounds (px-extent x y mw rh) element)
                     (setf rh (max rh eh))
                     (let ((ey (ecase (align layout)
                                 (:start y)
                                 (:end (- y eh)))))
                       (cond ((<= w (+ x ew ml mr))
                              ;; Row overflow, flush row.
                              (if (= x (+ ml ox))
                                  (setf (bounds element) (px-extent x ey (- w ml mr) eh))
                                  (decf i))
                              (ecase (align layout)
                                (:start (incf y (+ rh mu mb)))
                                (:end (decf y (+ rh mu mb))))
                              (setf x (+ ml ox))
                              (setf rh mh))
                             (T
                              (setf (bounds element) (px-extent x ey ew eh))
                              (incf x (+ ew ml mr))))))
                   (incf i)))))))
