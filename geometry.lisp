#|
 This file is a part of Alloy
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.alloy)

(defgeneric x (extent))
(defgeneric y (extent))
(defgeneric w (extent))
(defgeneric h (extent))
(defgeneric contained-p (point extent))

(defstruct (point (:constructor %point (x y)))
  (x 0.0f0 :type single-float)
  (y 0.0f0 :type single-float))

(defmethod print-object ((point point) stream)
  (format stream "~s" (list 'point (point-x point) (point-y point))))

(defun point (&optional (x 0) (y 0))
  (%point (float x 0f0) (float y 0f0)))

(define-compiler-macro point (&optional (x 0) (y 0) &environment env)
  (flet ((fold (arg)
           (if (constantp arg env)
               `(load-time-value (float ,arg 0f0))
               `(float ,arg 0f0))))
    `(%point ,(fold x) ,(fold y))))

(defmethod x ((point point)) (point-x point))
(defmethod y ((point point)) (point-y point))

(defun point= (a b)
  (and (= (point-x a) (point-x b))
       (= (point-y a) (point-y b))))

(defstruct (extent (:include point)
                   (:constructor %extent (x y w h)))
  (w 0.0f0 :type single-float)
  (h 0.0f0 :type single-float))

(defmethod print-object ((extent extent) stream)
  (format stream "~s" (list 'extent (extent-x extent) (extent-y extent) (extent-w extent) (extent-h extent))))

(defun extent (&optional (x 0) (y 0) (w 0) (h 0))
  (%extent (float x 0f0) (float y 0f0) (float w 0f0) (float h 0f0)))

(define-compiler-macro extent (&optional (x 0) (y 0) (w 0) (h 0) &environment env)
  (flet ((fold (arg)
           (if (constantp arg env)
               `(load-time-value (float ,arg 0f0))
               `(float ,arg 0f0))))
    `(%extent ,(fold x) ,(fold y) ,(fold w) ,(fold h))))

(defun extent= (a b)
  (and (= (extent-x a) (extent-x b))
       (= (extent-y a) (extent-y b))
       (= (extent-w a) (extent-w b))
       (= (extent-h a) (extent-h b))))

(defmethod x ((extent extent)) (extent-x extent))
(defmethod y ((extent extent)) (extent-y extent))
(defmethod w ((extent extent)) (extent-w extent))
(defmethod h ((extent extent)) (extent-h extent))

(defmethod contained-p ((point point) (extent extent))
  (and (<= 0 (- (point-x point) (extent-x extent)) (extent-w extent))
       (<= 0 (- (point-y point) (extent-y extent)) (extent-h extent))))

(defmethod contained-p ((inner extent) (outer extent))
  (and (<= 0 (- (extent-x inner) (extent-x outer)) (- (extent-w outer) (extent-w inner)))
       (<= 0 (- (extent-y inner) (extent-y outer)) (- (extent-h outer) (extent-h inner)))))

(defmacro destructure-extent ((&rest args &key x y w h) extent &body body)
  (declare (ignore x y w h))
  (let ((extentg (gensym "EXTENT")))
    `(let* ((,extentg ,extent)
            ,@(loop for (name func) in '((:x extent-x) (:y extent-y) (:w extent-w) (:h extent-h))
                    for var = (getf args name)
                    when var
                    collect `(,var (,func ,extentg))))
       ,@body)))

(defmacro with-extent ((&rest args &key x y w h) extent &body body)
  (declare (ignore x y w h))
  (let ((extentg (gensym "EXTENT")))
    `(let ((,extentg ,extent))
       (symbol-macrolet
           ,(loop for (name func) in '((:x extent-x) (:y extent-y) (:w extent-w) (:h extent-h))
                  for var = (getf args name)
                  when var
                  collect `(,var (,func ,extentg)))
         ,@body))))
