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
  (macrolet ((thunk ()
               (labels ((a (r c) `(aref a (+ ,c (* ,r 3))))
                        (b (r c) `(aref b (+ ,c (* ,r 3))))
                        (r (r c) `(aref r (+ ,c (* ,r 3))))
                        (cell (r c)
                          `(setf ,(r r c) (+ ,@(loop for rb from 0 below 3
                                                     collect `(* ,(a r rb) ,(b rb c)))))))
                 (list* 'progn
                        (loop for r from 0 below 3
                              append (loop for c from 0 below 3
                                           collect (cell r c)))))))
    (thunk)
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
    (setf (transform-matrix renderer) (mat* ex ex new))))

(defmethod translate ((renderer transformed-renderer) (point alloy:point))
  (with-matrix (mat 1 0 (alloy:pxx point)
                    0 1 (alloy:pxy point)
                    0 0 1)
    (add-matrix renderer mat)))

(defmethod translate ((renderer transformed-renderer) (extent alloy:extent))
  (with-matrix (mat 1 0 (alloy:pxx extent)
                    0 1 (alloy:pxy extent)
                    0 0 1)
    (add-matrix renderer mat)))

(defmethod translate ((renderer transformed-renderer) (margins alloy:margins))
  (with-matrix (mat 1 0 (alloy:pxl margins)
                    0 1 (alloy:pxb margins)
                    0 0 1)
    (add-matrix renderer mat)))

(defmethod scale ((renderer transformed-renderer) (size alloy:size))
  (with-matrix (mat (alloy:pxw size) 0 0
                    0 (alloy:pxh size) 0
                    0 0 1)
    (add-matrix renderer mat)))

(defmethod scale ((renderer transformed-renderer) (margins alloy:margins))
  (scale renderer (alloy:ensure-extent margins)))

(defmethod rotate ((renderer transformed-renderer) (phi float))
  (with-matrix (mat (cos phi) (- (sin phi)) 0
                    (sin phi) (cos phi) 0
                    0 0 1)
    (add-matrix renderer mat)))

(defmethod z-index ((renderer transformed-renderer))
  (aref (transform-matrix renderer) 8))

(defmethod (setf z-index) (z-index (renderer transformed-renderer))
  (setf (aref (transform-matrix renderer) 8) (float z-index)))
