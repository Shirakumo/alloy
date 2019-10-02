#|
 This file is a part of Alloy
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.alloy)

(defclass scrollbar (slider)
  ())

(defmethod initialize-instance :after ((scrollbar scrollbar) &key)
  (on (setf bounds) (bounds (data scrollbar))
    (declare (ignore bounds))
    (mark-for-render scrollbar)))

(defclass x-scrollbar (scrollbar)
  ())

(defmethod value ((scrollbar x-scrollbar))
  (pxx (offset (data scrollbar))))

(defmethod (setf value) (value (scrollbar x-scrollbar))
  (setf (offset (data scrollbar))
        (point value (pxy (offset (data scrollbar))))))

(defmethod range ((scrollbar scrollbar))
  (let ((layout (data scrollbar)))
    (if (inner layout)
        (with-unit-parent layout
          (cons (min 0 (- (pxw (bounds (inner layout))) (pxw (bounds layout)))) 0))
        '(0 . 1))))

(defclass y-scrollbar (scrollbar)
  ())

(defmethod value ((scrollbar y-scrollbar))
  (pxy (offset (data scrollbar))))

(defmethod (setf value) (value (scrollbar y-scrollbar))
  (setf (offset (data scrollbar))
        (point (pxx (offset (data scrollbar))) value)))

(defmethod range ((scrollbar scrollbar))
  (let ((layout (data scrollbar)))
    (if (inner layout)
        (with-unit-parent layout
          (cons (min 0 (- (pxh (bounds (inner layout))) (pxh (bounds layout)))) 0))
        '(0 . 1))))
