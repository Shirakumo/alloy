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

(defmethod (setf align) :after (value (layout linear-layout))
  (fit-linear-layout-contents layout (bounds layout)))

(defmethod (setf stretch) :after (value (layout linear-layout))
  (fit-linear-layout-contents layout (bounds layout)))

(defmethod (setf min-size) :after (value (layout linear-layout))
  (fit-linear-layout-contents layout (bounds layout)))

(defmethod (setf bounds) :after (extent (layout linear-layout))
  (fit-linear-layout-contents layout extent))

(defmethod resize :after ((layout linear-layout) x y)
  (fit-linear-layout-contents layout (bounds layout)))

;; TODO: Update this code to be more efficient and only consider updating the bounds of elements
;;       that are actively affected by the change.
(defmethod notice-size ((element layout-element) (layout linear-layout))
  (let* ((old (bounds layout))
         (new (fit-linear-layout-contents layout old)))
    (when (or (/= (pxh old) (pxh new))
              (/= (pxw old) (pxw new)))
      (unless (eq layout (layout-parent layout))
        (notice-size layout (layout-parent layout))))))

(defmethod leave :after ((element layout-element) (layout linear-layout))
  (fit-linear-layout-contents layout (bounds layout)))

(defclass vertical-linear-layout (linear-layout)
  ((align :initform :end)))

(defmethod suggest-size (size (layout vertical-linear-layout))
  (with-unit-parent layout
    (destructure-margins (:l l :u u :r r :b b :to-px T) (cell-margins layout)
      (let* ((w (pxw size))
             (h (pxh size))
             (th 0.0)
             (mh (pxh (min-size layout)))
             (mw (if (stretch layout) (- w l r) (pxw (min-size layout))))
             (y (ecase (align layout)
                  (:start b)
                  (:end (- h u)))))
        (do-elements (element layout :result (px-size w th))
          (let* ((size (suggest-size (px-size mw mh) element))
                 (ew (pxw size))
                 (eh (max mh (pxh size))))
            (setf w (max w (+ ew l r)))
            (incf th (+ eh u b))
            (ecase (align layout)
              (:start (incf y (+ eh u b)))
              (:end (decf y (+ eh u b))))))))))

(defmethod fit-linear-layout-contents ((layout vertical-linear-layout) extent)
  (with-unit-parent layout
    (destructure-margins (:l l :u u :r r :b b :to-px T) (cell-margins layout)
      (destructure-extent (:w w :h h :to-px T) extent
        (let ((mh (pxh (min-size layout)))
              (mw (if (stretch layout) (- w l r) (pxw (min-size layout))))
              (y (ecase (align layout)
                   (:start b)
                   (:end (- h u))))
              (x l))
          (do-elements (element layout :result (px-extent x 0 w y))
            (let* ((size (suggest-size (px-size mw mh) element))
                   (ew (pxw size))
                   (eh (max mh (pxh size))))
              (setf (bounds element)
                    (px-extent x
                               (ecase (align layout)
                                 (:start y)
                                 (:end (- y (+ eh u b))))
                               (max mw (if (stretch layout) (- w l r) (min (- w l r) ew)))
                               eh))
              (ecase (align layout)
                (:start (incf y (+ u b (pxh (bounds element)))))
                (:end (decf y (+ u b (pxh (bounds element)))))))))))))

(defclass horizontal-linear-layout (linear-layout)
  ())

(defmethod suggest-size (size (layout horizontal-linear-layout))
  (with-unit-parent layout
    (destructure-margins (:l l :u u :r r :b b :to-px T) (cell-margins layout)
      (let* ((w (pxw size))
             (h (pxh size))
             (tw 0.0)
             (mw (pxw (min-size layout)))
             (mh (if (stretch layout) (- h u b) (pxh (min-size layout))))
             (x (ecase (align layout)
                  (:start l)
                  (:end (- w r)))))
        (do-elements (element layout :start 0 :result (px-size tw h))
          (let* ((size (suggest-size (px-size mw h) element))
                 (ew (pxw size))
                 (eh (pxh size)))
            (setf h (max h (+ eh u b)))
            (incf tw (+ ew l r))
            (ecase (align layout)
              (:start (incf x (+ ew l r)))
              (:end (decf x (+ ew l r))))))))))

(defmethod fit-linear-layout-contents ((layout horizontal-linear-layout) extent)
  (with-unit-parent layout
    (destructure-margins (:l l :u u :r r :b b :to-px T) (cell-margins layout)
      (destructure-extent (:w w :h h :to-px T) extent
        (let ((mw (pxw (min-size layout)))
              (mh (if (stretch layout) (- h u b) (pxh (min-size layout))))
              (x (ecase (align layout)
                   (:start l)
                   (:end (- w r))))
              (y b))
          (do-elements (element layout :result (px-extent 0 y x h))
            (let* ((size (suggest-size (px-size mw mh) element))
                   (eh (pxh size))
                   (ew (max (pxw size) mw)))
              (setf (bounds element)
                    (px-extent (ecase (align layout)
                                 (:start x)
                                 (:end (- x (+ ew l r))))
                               y
                               ew
                               (max mh (if (stretch layout) (- h u b) (min (- h u b) eh)))))
              (ecase (align layout)
                (:start (incf x (+ l r (pxw (bounds element)))))
                (:end (decf x (+ l r (pxw (bounds element)))))))))))))
