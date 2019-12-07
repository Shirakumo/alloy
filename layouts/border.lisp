#|
 This file is a part of Alloy
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.alloy)

(defclass border-layout (layout container)
  ((l :initform NIL :accessor l)
   (u :initform NIL :accessor u)
   (r :initform NIL :accessor r)
   (b :initform NIL :accessor b)
   (c :initform NIL :accessor c)
   (padding :initarg :padding :initform (margins) :accessor padding)))

(defun border-place-slot (place)
  (ecase place
    (:west 'l)
    (:north 'u)
    (:east 'r)
    (:south 'b)
    (:center 'c)))

(defmethod enter ((element layout-element) (layout border-layout) &key (place :center) (size (un 20)))
  (let ((slot (border-place-slot place)))
    (when (slot-value layout slot)
      (cerror "Replace the element" 'place-already-occupied
              :element element :place place :layout layout :existing (slot-value layout slot)))
    (setf (slot-value layout slot) (list element size))))

(defmethod leave ((element layout-element) (layout border-layout))
  (flet ((test (slot)
           (when (eq element (car (slot-value layout slot)))
             (setf (slot-value layout slot) NIL))))
    (mapc #'test '(l u r b c))
    element))

(defmethod update ((element layout-element) (layout border-layout) &key place size)
  (let ((slot (border-place-slot place)))
    (when (slot-value layout slot)
      (cerror "Replace the element" 'place-already-occupied
              :element element :place place :layout layout :existing (slot-value layout slot)))
    (flet ((test (slot)
             (when (eq element (car (slot-value layout slot)))
               (unless size (setf size (second (slot-value layout slot))))
               (setf (slot-value layout slot) NIL))))
      (mapc #'test '(l u r b c)))
    (setf (slot-value layout slot) (list element size))))

(defmethod element-count ((layout border-layout))
  (loop for i in '(l u r b c)
        sum (if (slot-value layout i) 1 0)))

(defmethod elements ((layout border-layout))
  (let ((elements ()))
    (flet ((test (slot)
             (when (slot-value layout slot)
               (push (car (slot-value layout slot)) elements))))
      (mapc #'test '(l u r b c))
      elements)))

(defmethod element-index ((element layout-element) (layout border-layout))
  (cond ((eq element (car (slot-value layout 'l))) :west)
        ((eq element (car (slot-value layout 'u))) :north)
        ((eq element (car (slot-value layout 'r))) :east)
        ((eq element (car (slot-value layout 'b))) :south)
        ((eq element (car (slot-value layout 'c))) :center)))

(defmethod index-element (index (layout border-layout))
  (ecase index
    (:west (car (slot-value layout 'l)))
    (:north (car (slot-value layout 'u)))
    (:east (car (slot-value layout 'r)))
    (:south (car (slot-value layout 'b)))
    (:center (car (slot-value layout 'c)))))

(defmethod call-with-elements (function (layout border-layout) &key start end)
  (declare (ignore start end))
  (flet ((test (slot)
           (when (slot-value layout slot)
             (funcall function (car (slot-value layout slot))))))
    (mapc #'test '(l u r b c))))

(defmethod clear ((layout border-layout))
  (flet ((test (slot) (setf (slot-value layout slot) NIL)))
    (mapc #'test '(l u r b c))
    layout))

(defmethod notice-bounds ((element layout-element) (layout border-layout))
  (setf (bounds layout) (bounds layout)))

(defmethod suggest-bounds (extent (layout border-layout))
  (macrolet ((with-border (slot &body body)
               `(destructuring-bind (&optional element size) (slot-value layout ,slot)
                  (when element ,@body))))
    (with-unit-parent layout
      (let ((w 0) (h 0) (x (pxx extent)) (y (pxy extent)))
        (with-border 'b
          (incf h (pxh (suggest-bounds (px-extent x y (w extent) size) element)))
          (incf y h))
        (dolist (border '(l c r))
          (with-border border
            (let ((bounds (suggest-bounds (px-extent x y size (- (pxh extent) h)) element)))
              (incf w (pxw bounds))
              (incf x (pxw bounds))
              (setf h (max h (+ (- y (pxy extent)) (pxh bounds)))))))
        (setf y (+ (pxy extent) h))
        (with-border 'u
          (incf h (pxh (suggest-bounds (px-extent x y (w extent) size) element)))
          (incf y h))
        (px-extent (pxx extent) (pxy extent) w h)))))

(defmethod (setf bounds) :after (extent (layout border-layout))
  (macrolet ((with-border (slot &body body)
               `(destructuring-bind (&optional element size) (slot-value layout ,slot)
                  (let ((size (or size (un 0))))
                    (when element ,@body)))))
    (with-unit-parent layout
      (let ((w (pxw extent)) (h (pxh extent)) (x (pxx extent)) (y (pxy extent))
            (p (padding layout)))
        (incf x (pxl p))
        (incf y (pxb p))
        (decf w (+ (pxl p) (pxr p)))
        (decf h (+ (pxb p) (pxu p)))
        (with-border 'b
          (let ((diff (umax (pxh (suggest-bounds (px-extent x y w size) element)))))
            (setf (bounds element) (px-extent x y w diff))
            (decf h diff)
            (incf y diff)))
        (with-border 'u
          (let ((diff (umax size (pxh (suggest-bounds (px-extent x y w size) element)))))
            (setf (bounds element) (px-extent x (+ y (- h diff)) w diff))
            (decf h diff)))
        (with-border 'l
          (let ((diff (umax size (pxw (suggest-bounds (px-extent x y size h) element)))))
            (setf (bounds element) (px-extent x y diff h))
            (decf w diff)
            (incf x diff)))
        (with-border 'r
          (let ((diff (umax size (pxw (suggest-bounds (px-extent x y size h) element)))))
            (setf (bounds element) (px-extent (+ x (- w diff)) y diff h))
            (decf w diff)))
        (with-border 'c
          (setf (bounds element) (px-extent x y w h)))))))
