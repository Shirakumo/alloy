(in-package #:org.shirakumo.alloy)

(defclass scrollbar (slider)
  ((step :initform 20)))

(defmethod initialize-instance :after ((scrollbar scrollbar) &key)
  (on bounds (bounds (data scrollbar))
    (declare (ignore bounds))
    (mark-for-render scrollbar)))

(defclass x-scrollbar (scrollbar)
  ((orientation :initform :horizontal)))

(defmethod value ((scrollbar x-scrollbar))
  (- (unit-value (x (offset (data scrollbar))))))

(defmethod (setf value) (value (scrollbar x-scrollbar))
  (setf (offset (data scrollbar))
        (px-point (- value) (y (offset (data scrollbar))))))

(defmethod range ((scrollbar x-scrollbar))
  (let ((layout (data scrollbar)))
    (if (inner layout)
        (with-unit-parent layout
          (cons 0 (max 0 (- (pxw (bounds (inner layout))) (pxw (bounds layout))))))
        '(0 . 0))))

(defclass y-scrollbar (scrollbar)
  ((orientation :initform :vertical)))

(defmethod value ((scrollbar y-scrollbar))
  (- (unit-value (y (offset (data scrollbar))))))

(defmethod (setf value) (value (scrollbar y-scrollbar))
  (setf (offset (data scrollbar))
        (px-point (x (offset (data scrollbar))) (- value))))

(defmethod range ((scrollbar y-scrollbar))
  (let ((layout (data scrollbar)))
    (if (inner layout)
        (with-unit-parent layout
          (cons 0 (max 0 (- (pxh (bounds (inner layout))) (pxh (bounds layout))))))
        '(0 . 0))))
