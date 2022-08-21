#|
 This file is a part of Alloy
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.alloy)

(defclass clip-view (layout single-container observable)
  ((offset :initarg :offset :initform (px-point 0 0) :accessor offset)
   (stretch :initarg :stretch :initform T :accessor stretch)
   (limit :initarg :limit :initform NIL :accessor limit)))

(defmethod suggest-size (size (layout clip-view))
  (if (inner layout)
      (suggest-size size (inner layout))
      size))

(defmethod notice-size ((element layout-element) (layout clip-view))
  (update-clip-view layout))

(defun clamp-offset (offset layout)
  (flet ((clamp (l x u)
           (min (max l x) u)))
    (px-point (clamp (min 0 (- (pxw (bounds layout)) (pxw (bounds (inner layout))))) (pxx offset) 0)
              (clamp (min 0 (- (pxh (bounds layout)) (pxh (bounds (inner layout))))) (pxy offset) 0))))

(defmethod (setf offset) :around ((offset point) (layout clip-view))
  (let ((clamped (clamp-offset offset layout)))
    (unless (and (/= (pxx clamped) (pxx (offset layout)))
                 (/= (pxy clamped) (pxy (offset layout))))
      (call-next-method clamped layout))))

(defun update-clip-view (layout)
  (when (inner layout)
    (with-unit-parent layout
      (let* ((bounds (bounds layout))
             (ideal (suggest-size bounds (inner layout))))
        (setf (bounds (inner layout)) (px-extent 0 0
                                                 (cond ((null (stretch layout)) (w ideal))
                                                       ((eq :x (limit layout)) (w bounds))
                                                       (T (max (pxw ideal) (pxw bounds))))
                                                 (cond ((null (stretch layout)) (h ideal))
                                                       ((eq :y (limit layout)) (h bounds))
                                                       (T (max (pxh ideal) (pxh bounds)))))))
      ;; Ensure we clamp the offset into valid bounds.
      (setf (offset layout) (offset layout)))))

(defmethod (setf bounds) :after (bounds (layout clip-view))
  (update-clip-view layout))

(defmethod compute-global-position ((element clip-view))
  (with-unit-parent element
    (let ((x 0f0) (y 0f0))
      (loop for current = element then parent
            for parent = (layout-parent current)
            do (incf x (pxx (bounds current)))
               (incf y (pxy (bounds current)))
               (when (typep current 'clip-view)
                 (incf x (pxx (offset current)))
                 (incf y (pxy (offset current))))
            until (eql current parent))
      (values (- x (pxx (offset element)))
              (- y (pxy (offset element)))))))

(defmethod handle ((event scroll) (layout clip-view))
  (restart-case (call-next-method)
    (decline ()
      (let ((off (offset layout))
            (dx (case (limit layout)
                  (:x 0.0)
                  (:y (if (= 0.0 (dx event)) (dy event) (dx event)))
                  (T (dx event))))
            (dy (case (limit layout)
                  (:x (if (= 0.0 (dy event)) (dx event) (dy event)))
                  (:y 0.0)
                  (T (dy event)))))
        (setf (offset layout) (px-point (+ (* (pxw layout) 0.1 dx) (pxx off))
                                        (+ (* (pxh layout) 0.1 dy) (pxy off))))))))

(defmethod render ((renderer renderer) (layout clip-view))
  (when (inner layout)
    (constrain-visibility layout renderer)
    (translate renderer (offset layout))
    (render renderer (inner layout))))

(defmethod ensure-visible ((element layout-element) (layout clip-view))
  (let* ((view (bounds layout))
         (element (bounds element))
         (vyb (- (pxy (offset layout))))
         (vyt (- (pxh view) (pxy (offset layout)))))
    ;; FIXME: This does **not** work correctly for nested layouts.
    ;; KLUDGE: terrible kludge for Y only for now.
    (cond ((< (pxy element) vyb)
           (setf (offset layout) (px-point 0.0 (- (pxy element)))))
          ((< vyt (+ (pxy element) (pxh element)))
           (setf (offset layout) (px-point 0.0 (- (pxh view) (pxy element) (pxh element)))))))
  (call-next-method))
