#|
 This file is a part of Alloy
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.alloy.renderers.simple)

(defun matrix (&rest values)
  (let ((matrix (make-array 9 :element-type 'single-float)))
    (map-into matrix (lambda (x) (float x 0f0)) values)))

(defmacro with-matrix ((mat &rest els) &body body)
  `(let ((,mat (make-array 9 :element-type 'single-float)))
     (declare (dynamic-extent ,mat))
     ,@(loop for el in els
             for i from 0
             collect `(setf (aref ,mat ,i) (float ,el 0f0)))
     ,@body))

(defun matrix-identity ()
  (matrix 1 0 0
          0 1 0
          0 0 1))

(define-compiler-macro matrix (&rest values &environment env)
  (flet ((fold (arg)
           (if (constantp arg env)
               `(load-time-value (float ,arg 0f0))
               `(float ,arg 0f0))))
    (let ((matrix (gensym "MATRIX")))
      `(let ((,matrix (make-array 9 :element-type 'single-float)))
         (declare (optimize speed))
         ,@(loop for value in values
                 for i from 0
                 collect `(setf (aref ,matrix ,i) ,(fold value)))
         ,matrix))))

(defun mat* (r a b)
  (declare (type (simple-array single-float (9)) r a b))
  (declare (optimize speed (safety 1)))
  (let ((a00 (aref a 0))
        (a10 (aref a 1))
        (a20 (aref a 2))
        (a01 (aref a 3))
        (a11 (aref a 4))
        (a21 (aref a 5))
        (a02 (aref a 6))
        (a12 (aref a 7))
        (a22 (aref a 8))
        (b00 (aref b 0))
        (b10 (aref b 1))
        (b20 (aref b 2))
        (b01 (aref b 3))
        (b11 (aref b 4))
        (b21 (aref b 5))
        (b02 (aref b 6))
        (b12 (aref b 7))
        (b22 (aref b 8)))
    (setf (aref r 0) (+ (* a00 b00) (* a10 b01) (* a20 b02)))
    (setf (aref r 1) (+ (* a00 b10) (* a10 b11) (* a20 b12)))
    (setf (aref r 2) (+ (* a00 b20) (* a10 b21) (* a20 b22)))
    
    (setf (aref r 3) (+ (* a01 b00) (* a11 b01) (* a21 b02)))
    (setf (aref r 4) (+ (* a01 b10) (* a11 b11) (* a21 b12)))
    (setf (aref r 5) (+ (* a01 b20) (* a11 b21) (* a21 b22)))

    (setf (aref r 6) (+ (* a02 b00) (* a12 b01) (* a22 b02)))
    (setf (aref r 7) (+ (* a02 b10) (* a12 b11) (* a22 b12)))
    (setf (aref r 8) (+ (* a02 b20) (* a12 b21) (* a22 b22)))
    r))

(defclass transformed-renderer (renderer)
  ((transform-matrix :initform (matrix-identity) :accessor transform-matrix)))

(defmethod call-with-pushed-transforms (function (renderer transformed-renderer))
  (let ((current (transform-matrix renderer))
        (new (make-array 9 :element-type 'single-float)))
    (declare (dynamic-extent new))
    (declare (type (simple-array single-float (9)) current new))
    (dotimes (i 9) (setf (aref new i) (aref current i)))
    (setf (transform-matrix renderer) new)
    (unwind-protect
         (funcall function)
      (setf (transform-matrix renderer) current))))

(defmethod add-matrix ((renderer transformed-renderer) new)
  (let ((ex (transform-matrix renderer)))
    (setf (transform-matrix renderer) (mat* ex ex new))
    renderer))

(defun translate-by (renderer pxx pxy)
  (let* ((matrix (transform-matrix renderer)))
    (incf (aref matrix 2) (+ (* (aref matrix 0) pxx)
                             (* (aref matrix 1) pxy)))
    (incf (aref matrix 5) (+ (* (aref matrix 3) pxx)
                             (* (aref matrix 4) pxy)))
    (incf (aref matrix 8) (+ (* (aref matrix 6) pxx)
                             (* (aref matrix 7) pxy)))
    renderer))

(defun scale-by (renderer pxw pxh)
  (let* ((matrix (transform-matrix renderer)))
    (setf (aref matrix 0) (* (aref matrix 0) pxw))
    (setf (aref matrix 1) (* (aref matrix 1) pxh))
    (setf (aref matrix 4) (* (aref matrix 4) pxw))
    (setf (aref matrix 5) (* (aref matrix 5) pxh))
    (setf (aref matrix 7) (* (aref matrix 7) pxw))
    (setf (aref matrix 8) (* (aref matrix 8) pxh))
    renderer))

(defmethod translate ((renderer transformed-renderer) (point alloy:point))
  (translate-by renderer (alloy:pxx point) (alloy:pxy point)))

(defmethod translate ((renderer transformed-renderer) (extent alloy:extent))
  (translate-by renderer (alloy:pxx extent) (alloy:pxy extent)))

(defmethod translate ((renderer transformed-renderer) (margins alloy:margins))
  (translate-by renderer (alloy:pxl margins) (alloy:pxb margins)))

(defmethod scale ((renderer transformed-renderer) (size alloy:size))
  (scale-by renderer (alloy:pxw size) (alloy:pxh size)))

(defmethod scale ((renderer transformed-renderer) (margins alloy:margins))
  (scale renderer (alloy:ensure-extent margins)))

(defmethod rotate ((renderer transformed-renderer) (phi float))
  (let ((cos (float (cos phi) 0f0))
        (sin (float (sin phi) 0f0)))
    (with-matrix (mat cos (- sin) 0
                      sin cos 0
                      0 0 1)
      (add-matrix renderer mat))))

(defmethod z-index ((renderer transformed-renderer))
  (aref (transform-matrix renderer) 8))

(defmethod (setf z-index) (z-index (renderer transformed-renderer))
  (setf (aref (transform-matrix renderer) 8) (float z-index)))
