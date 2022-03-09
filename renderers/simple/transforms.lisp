#|
 This file is a part of Alloy
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.alloy.renderers.simple)

(defun matrix (&rest values)
  (let ((matrix (make-array 9 :element-type 'single-float)))
    (map-into matrix (lambda (x) (float x 0f0)) values)))

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
  (let ((current (transform-matrix renderer)))
    (setf (transform-matrix renderer) (copy-seq current))
    (unwind-protect
         (funcall function)
      (setf (transform-matrix renderer) current))))

(defmethod add-matrix ((renderer transformed-renderer) new)
  (let ((ex (transform-matrix renderer)))
    (setf (transform-matrix renderer) (mat* ex ex new))))

(defmethod translate ((renderer transformed-renderer) (point alloy:point))
  (add-matrix renderer (matrix 1 0 (alloy:pxx point)
                               0 1 (alloy:pxy point)
                               0 0 1)))

(defmethod translate ((renderer transformed-renderer) (extent alloy:extent))
  (add-matrix renderer (matrix 1 0 (alloy:pxx extent)
                               0 1 (alloy:pxy extent)
                               0 0 1)))

(defmethod translate ((renderer transformed-renderer) (margins alloy:margins))
  (add-matrix renderer (matrix 1 0 (alloy:pxl margins)
                               0 1 (alloy:pxb margins)
                               0 0 1)))

(defmethod scale ((renderer transformed-renderer) (size alloy:size))
  (add-matrix renderer (matrix (alloy:pxw size) 0 0
                               0 (alloy:pxh size) 0
                               0 0 1)))

(defmethod scale ((renderer transformed-renderer) (margins alloy:margins))
  (scale renderer (alloy:ensure-extent margins)))

(defmethod rotate ((renderer transformed-renderer) (phi float))
  (add-matrix renderer (matrix (cos phi) (- (sin phi)) 0
                               (sin phi) (cos phi) 0
                               0 0 1)))

(defmethod z-index ((renderer transformed-renderer))
  (aref (transform-matrix renderer) 8))

(defmethod (setf z-index) (z-index (renderer transformed-renderer))
  (setf (aref (transform-matrix renderer) 8) (float z-index)))
