#|
 This file is a part of Alloy
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.alloy)

(defgeneric x (geometry))
(defgeneric y (geometry))
(defgeneric w (geometry))
(defgeneric h (geometry))
(declaim (ftype (function (T) unit) x y w h))
(defgeneric contained-p (point extent))

(defstruct (point (:constructor %point (x y)))
  (x NIL :type unit)
  (y NIL :type unit))

(defmethod print-object ((point point) stream)
  (format stream "~s" (list 'point (point-x point) (point-y point))))

(defmethod make-load-form ((point point) &optional env)
  (declare (ignore env))
  (list '%point (point-x point) (point-y point)))

(defun point (&optional (x 0) (y 0))
  (%point (unit x) (unit y)))

(defmethod x ((point point)) (point-x point))
(defmethod y ((point point)) (point-y point))

(defun point= (a b)
  (and (u= (point-x a) (point-x b))
       (u= (point-y a) (point-y b))))

(defstruct (size (:constructor %size (w h)))
  (w NIL :type unit)
  (h NIL :type unit))

(defmethod print-object ((size size) stream)
  (format stream "~s" (list 'size (size-w size) (size-h size))))

(defmethod make-load-form ((size size) &optional env)
  (declare (ignore env))
  (list '%size (size-w size) (size-h size)))

(defun size (&optional (w 0) h)
  (if h
      (%size (unit w) (unit h))
      (%size (unit w) (unit w))))

(defmethod w ((size size)) (size-w size))
(defmethod h ((size size)) (size-h size))

(defun size= (a b)
  (and (u= (size-w a) (size-w b))
       (u= (size-h a) (size-h b))))

(defstruct (margins (:constructor %margins (l u r b)))
  (l NIL :type unit)
  (u NIL :type unit)
  (r NIL :type unit)
  (b NIL :type unit))

(defmethod print-object ((margins margins) stream)
  (format stream "~s" (list 'margins (margins-l margins) (margins-u margins) (margins-r margins) (margins-b margins))))

(defmethod make-load-form ((margins margins) &optional env)
  (declare (ignore env))
  (list '%margins (margins-l margins) (margins-u margins) (margins-r margins) (margins-b margins)))

(defun margins (&optional (l 0) u r b)
  (cond (b (%margins (unit l) (unit u) (unit r) (unit b)))
        (r (%margins (unit l) (unit u) (unit r) (unit 0)))
        (u (%margins (unit l) (unit u) (unit l) (unit u)))
        (l (%margins (unit l) (unit l) (unit l) (unit l)))))

(defun margins= (a b)
  (and (u= (margins-l a) (margins-l b))
       (u= (margins-u a) (margins-u b))
       (u= (margins-r a) (margins-r b))
       (u= (margins-b a) (margins-b b))))

(defmacro destructure-margins ((&rest args &key l u r b to-px) margins &body body)
  (declare (ignore l u r b))
  (let ((marginsg (gensym "EXTENT")))
    `(let* ((,marginsg ,margins)
            ,@(loop for (name func) in '((:l margins-l) (:u margins-u) (:r margins-r) (:b margins-b))
                    for var = (getf args name)
                    when var
                    collect `(,var (,(if to-px 'to-px 'identity) (,func ,marginsg)))))
       ,@body)))

(defstruct (extent (:include point)
                   (:constructor %extent (x y w h)))
  (w NIL :type unit)
  (h NIL :type unit))

(defmethod print-object ((extent extent) stream)
  (format stream "~s" (list 'extent (extent-x extent) (extent-y extent) (extent-w extent) (extent-h extent))))

(defmethod make-load-form ((extent extent) &optional env)
  (declare (ignore env))
  (list '%extent (extent-x extent) (extent-y extent) (extent-w extent) (extent-h extent)))

(defun extent (&optional (x 0) (y 0) (w 0) (h 0))
  (%extent (unit x) (unit y) (unit w) (unit h)))

(defun extent= (a b)
  (and (u= (extent-x a) (extent-x b))
       (u= (extent-y a) (extent-y b))
       (u= (extent-w a) (extent-w b))
       (u= (extent-h a) (extent-h b))))

(defmethod x ((extent extent)) (extent-x extent))
(defmethod y ((extent extent)) (extent-y extent))
(defmethod w ((extent extent)) (extent-w extent))
(defmethod h ((extent extent)) (extent-h extent))

(defmethod contained-p ((point point) (extent extent))
  (and (u<= 0 (u- (point-x point) (extent-x extent)) (extent-w extent))
       (u<= 0 (u- (point-y point) (extent-y extent)) (extent-h extent))))

(defmethod contained-p ((inner extent) (outer extent))
  (and (u<= 0 (u- (extent-x inner) (extent-x outer)) (u- (extent-w outer) (extent-w inner)))
       (u<= 0 (u- (extent-y inner) (extent-y outer)) (u- (extent-h outer) (extent-h inner)))))

(defun absolute-extent (extent &optional (parent *unit-parent*))
  (with-unit-parent parent
    (flet ((transform (unit)
             (etypecase unit
               (px unit)
               (unit (px (to-px unit))))))
      (extent (transform (extent-x extent))
              (transform (extent-y extent))
              (transform (extent-w extent))
              (transform (extent-h extent))))))

(defmacro destructure-extent ((&rest args &key x y w h to-px) extent &body body)
  (declare (ignore x y w h))
  (let ((extentg (gensym "EXTENT")))
    `(let* ((,extentg ,extent)
            ,@(loop for (name func) in '((:x extent-x) (:y extent-y) (:w extent-w) (:h extent-h))
                    for var = (getf args name)
                    when var
                    collect `(,var (,(if to-px 'to-px 'identity) (,func ,extentg)))))
       ,@body)))
