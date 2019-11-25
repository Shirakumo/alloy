#|
 This file is a part of Alloy
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.alloy)

(defclass grid-layout (layout vector-container)
  ((stretch :initarg :stretch :initform T :accessor stretch)
   (cell-margins :initarg :cell-margins :initform (margins 5) :accessor cell-margins)
   (row-sizes :initform (make-array 0 :adjustable T :fill-pointer T) :reader row-sizes)
   (col-sizes :initform (make-array 0 :adjustable T :fill-pointer T) :reader col-sizes)))

(defmethod shared-initialize :after ((layout grid-layout) slots &key (row-sizes NIL r-p) (col-sizes NIL c-p))
  (when r-p (setf (row-sizes layout) row-sizes))
  (when c-p (setf (col-sizes layout) col-sizes)))

(defmethod (setf stretch) :after (value (layout grid-layout))
  (suggest-bounds (bounds layout) layout))

(defun coerce-grid-size (v)
  (etypecase v
    (real (un v))
    (unit v)
    ((eql T) T)))

(defmethod (setf row-sizes) ((value sequence) (layout grid-layout))
  (adjust-grid layout (length value) (length (col-sizes layout)))
  (adjust-array (row-sizes layout) (length value) :fill-pointer (length value))
  (map-into (row-sizes layout) #'coerce-grid-size value))

(defmethod (setf col-sizes) ((value sequence) (layout grid-layout))
  (adjust-grid layout (length (row-sizes layout)) (length value))
  (adjust-array (col-sizes layout) (length value) :fill-pointer (length value))
  (map-into (col-sizes layout) #'coerce-grid-size value))

(defmethod adjust-grid ((layout grid-layout) nrows ncols)
  (let ((orows (length (row-sizes layout)))
        (ocols (length (col-sizes layout)))
        (old (elements layout))
        (new (make-array (* nrows ncols) :initial-element NIL :adjustable T :fill-pointer 0)))
    ;; Leave old ones
    (loop for row from nrows below orows
          do (loop for col from ncols below ocols
                   do (leave (aref old (+ col (* row ocols))) layout)))
    ;; Populate new grid
    (loop for row from 0 below (min nrows orows)
          do (loop for col from 0 below (min ncols ocols)
                   for oidx = (+ col (* row ocols))
                   while (< oidx (fill-pointer old))
                   do (vector-push (aref old oidx) new)))
    ;; Update if possible
    (setf (slot-value layout 'elements) new)
    (when (slot-boundp layout 'layout-parent)
      (suggest-bounds (bounds layout) layout))))

(defmethod enter :before ((element layout-element) (layout grid-layout) &key row col)
  (when (and row col)
    (let ((idx (+ col (* row (length (col-sizes layout))))))
      (when (aref (elements layout) idx)
        (error 'place-already-occupied
               :element element :place (cons row col) :layout layout :existing (aref (elements layout) idx))))))

(defmethod enter ((element layout-element) (layout grid-layout) &key row col index)
  (call-next-method element layout :index (if (and row col)
                                              (+ col (* row (length (col-sizes layout))))
                                              index))
  ;; Extend rows as much as necessary
  (loop while (< (* (length (row-sizes layout)) (length (col-sizes layout)))
                 (fill-pointer (elements layout)))
        do (vector-push-extend (aref (row-sizes layout) (1- (length (row-sizes layout)))) (row-sizes layout))))

(defmethod leave ((element layout-element) (layout grid-layout))
  (setf (aref (elements layout) (element-index element layout)) NIL)
  element)

(defmethod update ((element layout-element) (layout grid-layout) &key row col index)
  (when (or index (and row col))
    (let ((idx (element-index element layout)))
      (setf (aref (elements layout) idx) NIL))
    (array-utils:vector-push-extend-position element (elements container)
                                             (or index (+ col (* row (length (col-sizes layout))))))
    ;; Extend rows as much as necessary
    (loop while (< (* (length (row-sizes layout)) (length (col-sizes layout)))
                   (fill-pointer (elements layout)))
          do (vector-push-extend (aref (row-sizes layout) (1- (length (row-sizes layout)))) (row-sizes layout))))
  element)

(defmethod call-with-elements (function (layout grid-layout) &key start end)
  (loop with elements = (elements layout)
        for i from (or start 0) below (or end (length elements))
        for el = (aref elements i)
        do (when el (funcall function el))))

(defmethod clear ((layout grid-layout))
  (loop for i downfrom (1- (length (elements layout))) to 0
        for element = (aref (elements layout) i)
        do (when element (leave element layout))))

(defmethod notice-bounds ((element layout-element) (layout grid-layout))
  (let ((updated (suggest-bounds (bounds layout) layout)))
    (unless (extent= (bounds layout) updated)
      (setf (bounds layout) updated))))

(defun spanning-size (sizes total)
  (let ((count 0)
        (total (to-px total)))
    (loop for size across sizes
          do (etypecase size
               (unit (decf total (to-px size)))
               ((eql T) (incf count))))
    (if (< 0 count)
        (/ total count)
        total)))

(defmethod (setf bounds) :after (extent (layout grid-layout))
  (with-unit-parent layout
    (destructure-margins (:l ml :u mu :r mr :b mb :to-px T) (cell-margins layout)
      (let ((th (spanning-size (row-sizes layout) (extent-h extent)))
            (tw (spanning-size (col-sizes layout) (extent-w extent))))
        (loop with elements = (elements layout)
              for y = (+ (pxy extent) (pxh extent) (- mb)) then (- y h)
              for hish across (row-sizes layout)
              for h = (if (eql T hish) th (to-px hish))
              for i from 0
              do (loop for x = (+ (pxx extent) ml) then (+ x w)
                       for wish across (col-sizes layout)
                       for w = (if (eql T wish) tw (to-px wish))
                       for j from 0
                       for idx from (* i (length (col-sizes layout)))
                       while (< idx (fill-pointer elements))
                       do (let ((element (aref elements idx)))
                            (when element
                              (let ((ideal (suggest-bounds (px-extent x y (- w ml mr) (- h mb mu)) element)))
                                (setf (bounds element)
                                      (px-extent x (- y (if (stretch layout) (- h mb mu) (pxh ideal)))
                                                 (if (stretch layout) (- w ml mr) (extent-w ideal))
                                                 (if (stretch layout) (- h mb mu) (extent-h ideal)))))))))))))

(defmethod suggest-bounds (extent (layout grid-layout))
  (with-unit-parent layout
    (destructure-margins (:l ml :u mu :r mr :b mb :to-px T) (cell-margins layout)
      (px-extent (extent-x extent)
                 (extent-y extent)
                 (loop for col across (col-sizes layout)
                       if (eql T col) sum (+ ml mr)
                       else sum (to-px col))
                 (loop for row across (row-sizes layout)
                       if (eql T row) sum (+ mu mb)
                       else sum (to-px row))))))
