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

(defstruct (extent (:constructor %make-extent (x y w h)))
  (x 0.0f0 :type single-float)
  (y 0.0f0 :type single-float)
  (w 0.0f0 :type single-float)
  (h 0.0f0 :type single-float))

(defun make-extent (&optional (x 0) (y 0) (w 0) (h 0))
  (%make-extent (float x 0f0) (float y 0f0) (float w 0f0) (float h 0f0)))

(define-compiler-macro make-extent (&optional (x 0) (y 0) (w 0) (h 0) &environment env)
  (flet ((fold (arg)
           (if (constantp arg env)
               `(load-time-value (float ,arg 0f0))
               `(float ,arg 0f0))))
    `(%make-extent ,(fold x) ,(fold y) ,(fold w) ,(fold h))))

(defmethod x ((extent extent)) (extent-x extent))
(defmethod y ((extent extent)) (extent-y extent))
(defmethod w ((extent extent)) (extent-w extent))
(defmethod h ((extent extent)) (extent-h extent))

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
           ,@(loop for (name func) in '((:x extent-x) (:y extent-y) (:w extent-w) (:h extent-h))
                   for var = (getf args name)
                   when var
                   collect `(,var (,func ,extentg)))
         ,@body))))
