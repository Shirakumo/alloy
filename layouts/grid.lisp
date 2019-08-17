#|
 This file is a part of Alloy
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.alloy)

(defclass grid-layout (layout)
  ((stretch :initarg :stretch :initform T :accessor stretch)
   (cell-margins :initarg :cell-margins :initform (margins) :accessor cell-margins)
   (row-sizes :initform #() :reader row-sizes)
   (col-sizes :initform #() :reader col-sizes)
   (elements :initform (make-array (list 0 0) :adjustable T :initial-element NIL) :reader elements)))

;; FIXME: allow stretch columns

(defmethod initialize-instance :after ((layout grid-layout) &key row-sizes col-sizes)
  (setf (row-sizes layout) row-sizes)
  (setf (col-sizes layout) col-sizes))

(defmethod (setf stretch) :after (value (layout grid-layout))
  (suggest-bounds (bounds layout) layout))

(defmethod (setf row-sizes) ((value sequence) (layout grid-layout))
  (setf (slot-value layout 'row-sizes) (map 'vector #'float value)))

(defmethod (setf col-sizes) ((value sequence) (layout grid-layout))
  (setf (slot-value layout 'col-sizes) (map 'vector #'float value)))

(defmethod (setf row-sizes) :after (value (layout grid-layout))
  (adjust-array (elements layout) (list (length (row-sizes layout))
                                        (length (col-sizes layout)))
                :initial-element NIL)
  (suggest-bounds (bounds layout) layout))

(defmethod (setf col-sizes) :after (value (layout grid-layout))
  (adjust-array (elements layout) (list (length (row-sizes layout))
                                        (length (col-sizes layout)))
                :initial-element NIL)
  (suggest-bounds (bounds layout) layout))

(defmethod enter :before ((element layout-element) (layout grid-layout) &key row col)
  (unless (and row col)
    (error "ROW and COL required."))
  (when (aref (elements layout) row col)
    (error "Cannot enter~%  ~a~%at ~a,~a into~%  ~a~%as it is already occupied by~%  ~a"
           element row col layout (aref (elements layout) row col))))

(defmethod enter ((element layout-element) (layout grid-layout) &key row col)
  (setf (aref (elements layout) row col) element)
  element)

(defmethod leave ((element layout-element) (layout grid-layout))
  (destructuring-bind (r c) (element-index element layout)
    (setf (aref (elements layout) r c) NIL))
  element)

(defmethod update ((element layout-element) (layout grid-layout) &key row col)
  (destructuring-bind (r c) (element-index element layout)
    (setf (aref (elements layout) r c) NIL))
  (setf (aref (elements layout) row col) element)
  element)

(defmethod call-with-elements (function (layout grid-layout) &key start end)
  (loop with elements = (elements layout)
        for i from (or start 0) below (or end (array-total-size elements))
        for element = (row-major-aref elements i)
        do (when element (funcall function element))))

(defmethod element-index ((element layout-element) (layout grid-layout))
  (let ((elements (elements layout)))
    (dotimes (i (length (row-sizes layout)))
      (dotimes (j (length (col-sizes layout)))
        (when (eq element (aref elements i j))
          (return-from element-index (list i j)))))))

(defmethod notice-bounds ((element layout-element) (layout grid-layout))
  (let ((updated (suggest-bounds (bounds layout) layout)))
    (unless (extent= (bounds layout) updated)
      (setf (bounds layout) updated))))

(defmethod (setf bounds) :after (extent (layout grid-layout))
  (destructure-margins (:l ml :u mu :r mr :b mb) (cell-margins layout)
    (loop with elements = (elements layout)
          for y = (+ (extent-y extent) mb) then (+ y h)
          for h across (row-sizes layout)
          for i from 0
          do (loop for x = (+ (extent-x extent) ml) then (+ x w)
                   for w across (col-sizes layout)
                   for j from 0
                   for element = (aref elements i j)
                   do (when element
                        (let ((ideal (suggest-bounds (extent x y (- w ml mr) (- h mb mu))
                                                     element))
                              (bounds (bounds element)))
                          (setf (extent-x bounds) x)
                          (setf (extent-y bounds) y)
                          (cond ((stretch layout)
                                 (setf (extent-w bounds) (- w ml mr))
                                 (setf (extent-h bounds) (- h mb mu)))
                                (T
                                 (setf (extent-w bounds) (extent-w ideal))
                                 (setf (extent-h bounds) (extent-h ideal))))))))))

(defmethod suggest-bounds (extent (layout grid-layout))
  (extent (extent-x extent)
          (extent-y extent)
          (loop for col across (col-sizes layout) sum col)
          (loop for row across (row-sizes layout) sum row)))
