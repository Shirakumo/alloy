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
  (setf (bounds layout) (bounds layout)))

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

(defmethod (setf offset) :after (offset (layout clip-view))
  (setf (bounds layout) (bounds layout)))

(defmethod (setf bounds) :after (bounds (layout clip-view))
  (when (inner layout)
    (with-unit-parent layout
      (let ((ideal (suggest-size bounds (inner layout))))
        (setf (bounds (inner layout)) (px-extent 0 0
                                                 (cond ((null (stretch layout)) (w ideal))
                                                       ((eq :x (limit layout)) (w bounds))
                                                       (T (max (pxw ideal) (pxw bounds))))
                                                 (cond ((null (stretch layout)) (h ideal))
                                                       ((eq :y (limit layout)) (h bounds))
                                                       (T (max (pxh ideal) (pxh bounds)))))))
      ;; Ensure we clamp the offset into valid bounds.
      ;;(setf (offset layout) (offset layout))
      )))

(defmethod handle ((event scroll) (layout clip-view))
  (restart-case (call-next-method)
    (decline ()
      (let ((off (offset layout)))
        (setf (offset layout) (px-point (+ (* (pxw layout) 0.1 (dx event)) (pxx off))
                                        (+ (* (pxh layout) 0.1 (dy event)) (pxy off))))))))

(defmethod render ((renderer renderer) (layout clip-view))
  (when (inner layout)
    (with-constrained-visibility (layout renderer)
      (translate renderer (offset layout))
      (render renderer (inner layout)))))

(defmethod ensure-visible ((element layout-element) (layout clip-view))
  (let* ((view (bounds layout))
         (container (bounds (inner layout)))
         (element (bounds element)))
    ;; KLUDGE: terrible kludge for Y only for now.
    (unless (and (<= (pxy view) (pxy element))
                 (<= (+ (pxy element) (pxh element)) (+ (pxy view) (pxh view))))
      (cond ((< (pxy element) (pxy view))
             (setf (offset layout) (px-point (pxx (offset layout))
                                             (- (- (pxy element) (pxy container)) (- (pxh container) (pxh view))))))
            (T
             (setf (offset layout) (px-point (pxx (offset layout))
                                             (+ (- (- (pxy element) (pxy container)) (- (pxh container) (pxh view)))
                                                (- (pxh view) (pxh element)))))))))
  (call-next-method))
