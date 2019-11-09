#|
 This file is a part of Alloy
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.alloy)

(defclass clip-view (layout observable)
  ((inner :initarg :inner :initform NIL :accessor inner)
   (offset :initarg :offset :initform (px-point 0 0) :accessor offset)
   (stretch :initarg :stretch :initform T :accessor stretch)))

(defmethod enter ((element layout-element) (layout clip-view) &key)
  (when (inner layout)
    (cerror "Replace the element" 'place-already-occupied
            :element element :place T :layout layout :existing (inner layout)))
  (setf (inner layout) element))

(defmethod update ((element layout-element) (layout clip-view) &key))

(defmethod leave ((element layout-element) (layout clip-view))
  (setf (inner layout) NIL))

(defmethod call-with-elements (function (layout clip-view) &key start end)
  (declare (ignore start end))
  (when (inner layout)
    (funcall function (inner layout))))

(defmethod suggest-bounds (extent (layout clip-view))
  (if (inner layout)
      (suggest-bounds extent (inner layout))
      extent))

(defmethod notice-bounds ((element layout-element) (layout clip-view))
  (setf (bounds layout) (bounds layout)))

(defmethod (setf offset) :after (offset (layout clip-view))
  (setf (bounds layout) (bounds layout)))

(defmethod (setf bounds) :after (bounds (layout clip-view))
  (when (inner layout)
    (with-unit-parent layout
      (let ((ideal (suggest-bounds (px-extent (- (pxx bounds) (pxx (offset layout)))
                                              (- (pxy bounds) (pxy (offset layout)))
                                              (w bounds)
                                              (h bounds))
                                   (inner layout))))
        (setf (bounds (inner layout)) (px-extent (- (pxx ideal) (- (pxw ideal) (pxw bounds)))
                                                 (- (pxy ideal) (- (pxh ideal) (pxh bounds)))
                                                 (if (stretch layout)
                                                     (max (pxw ideal) (pxw bounds))
                                                     (w ideal))
                                                 (if (stretch layout)
                                                     (max (pxh ideal) (pxh bounds))
                                                     (h ideal))))))))

(defmethod handle ((event scroll) (layout clip-view) ctx)
  (unless (call-next-method)
    (let ((off (offset layout))
          (bo (bounds layout))
          (bi (bounds (inner layout))))
      (setf (offset layout) (px-point (min (max 0 (+ (dx event) (pxx off))) (- (pxw bo) (pxw bi)))
                                      (min (max 0 (+ (dy event) (pxy off))) (- (pxh bo) (pxh bi))))))))

(defmethod render ((renderer renderer) (layout clip-view))
  (when (inner layout)
    (with-constrained-visibility ((bounds layout) renderer)
      (render renderer (inner layout)))))

(defmethod ensure-visible ((extent extent) (layout clip-view))
  (let* ((bounds (bounds layout))
         (hwe (/ (pxw extent) 2)) (hhe (/ (pxh extent) 2))
         (hwb (/ (pxw bounds) 2)) (hhb (/ (pxh bounds) 2))
         (cxe (+ (pxx extent) hwe)) (cye (+ (pxy extent) hhe))
         (cxb (+ (pxx bounds) hwb)) (cyb (+ (pxy bounds) hhb))
         (dx (- cxe cxb)) (dy (- cye cyb))
         (dw (- hwe hwb)) (dh (- hhe hhb))
         (shiftx (- (* dw (signum dx)) dx))
         (shifty (- (* dh (signum dy)) dy)))
    (setf (offset layout) (px-point (+ (pxx (offset layout)) shiftx)
                                    (+ (pxy (offset layout)) shifty))))
  (call-next-method))

;; FIXME: This might need to be a component instead.. ?
