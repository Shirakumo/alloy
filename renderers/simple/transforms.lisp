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

(defun mat* (a b)
  (let ((r (make-array 9 :element-type 'single-float)))
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
      r)))

(defclass transform ()
  ((clip-mask :accessor clip-mask)
   (transform-matrix :accessor transform-matrix)))

(defmethod shared-initialize :after ((transform transform) slots &key (clip-mask NIL c-p) transform-matrix)
  (when c-p (clip transform clip-mask))
  (when transform-matrix (setf (transform-matrix transform) transform-matrix)))

(defmethod initialize-instance :after ((transform transform) &key parent)
  (macrolet ((init-slot (slot)
               `(unless (slot-boundp transform ',slot)
                  (setf (,slot transform) (,slot parent)))))
    (init-slot clip-mask)
    (init-slot transform-matrix)))

(defmethod add-matrix ((transform transform) new)
  (setf (transform-matrix transform) (mat* (transform-matrix transform) new)))

(defmethod clip ((transform transform) (extent alloy:extent))
  (setf (clip-mask transform) extent))

(defmethod clip ((transform transform) (none null))
  (setf (clip-mask transform) NIL))

(defmethod translate ((transform transform) (point alloy:point))
  (add-matrix transform (matrix 1 0 (alloy:pxx point)
                                0 1 (alloy:pxy point)
                                0 0 1)))

(defmethod translate ((transform transform) (extent alloy:extent))
  (add-matrix transform (matrix 1 0 (alloy:pxx extent)
                                0 1 (alloy:pxy extent)
                                0 0 1)))

(defmethod translate ((transform transform) (margins alloy:margins))
  (add-matrix transform (matrix 1 0 (alloy:pxl margins)
                                0 1 (alloy:pxb margins)
                                0 0 1)))

(defmethod scale ((transform transform) (size alloy:size))
  (add-matrix transform (matrix (alloy:pxw size) 0 0
                                0 (alloy:pxh size) 0
                                0 0 1)))

(defmethod scale ((transform transform) (margins alloy:margins))
  (scale transform (alloy:ensure-extent margins)))

(defmethod rotate ((transform transform) (phi float))
  (add-matrix transform (matrix (cos phi) (- (sin phi)) 0
                                (sin phi) (cos phi) 0
                                0 0 1)))

(defmethod z-index ((transform transform))
  (aref (transform-matrix transform) 8))

(defmethod (setf z-index) (z-index (transform transform))
  (setf (aref (transform-matrix transform) 8) (float z-index)))

(defclass transformed-renderer (renderer)
  ((transform :accessor transform)))

(defmethod initialize-instance :after ((renderer transformed-renderer) &key)
  (setf (transform renderer) (make-default-transform renderer)))

(defgeneric make-default-transform (renderer))

(defmethod make-default-transform ((renderer transformed-renderer))
  (make-instance 'transform
                 :clip-mask NIL
                 :transform-matrix (matrix-identity)))

(defmethod call-with-pushed-transforms (function (renderer transformed-renderer))
  (let ((current (transform renderer)))
    (setf (transform renderer) (make-instance (class-of current) :parent current))
    (unwind-protect
         (funcall function)
      (setf (transform renderer) current))))

(defmethod clip ((renderer transformed-renderer) region)
  (clip (transform renderer) region))

(defmethod translate ((renderer transformed-renderer) point)
  (translate (transform renderer) point))

(defmethod scale ((renderer transformed-renderer) size)
  (scale (transform renderer) size))

(defmethod rotate ((renderer transformed-renderer) phi)
  (rotate (transform renderer) phi))

(defmethod z-index ((renderer transformed-renderer))
  (z-index (transform renderer)))

(defmethod (setf z-index) (z-index (renderer transformed-renderer))
  (setf (z-index (transform renderer)) z-index))
