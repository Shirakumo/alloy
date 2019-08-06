#|
 This file is a part of Alloy
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.alloy.renderer.simple)

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

(defmethod shared-initialize :after ((transform transform) slots &key (clip-mask NIL c-p) matrix)
  (when c-p (clip transform clip-mask))
  (when matrix (setf (transform-matrix transform) matrix)))

(defmethod initialize-instance :after ((transform transform) &key parent)
  (macrolet ((init-slot (slot)
               `(unless (slot-boundp transform ',slot)
                  (setf (,slot transform) (,slot parent)))))
    (init-slot clip-mask)
    (init-slot transform-matrix)))

(defmethod add-matrix ((transform transform) new)
  (setf (transform-matrix transform) (mat* (transform-matrix transform) new)))

(defmethod clip ((transform transform) (extent alloy:extent))
  (let ((target (clip-mask transform)))
    (if target
        (setf (alloy:extent-x target) (alloy:extent-x extent)
              (alloy:extent-y target) (alloy:extent-y extent)
              (alloy:extent-w target) (alloy:extent-w extent)
              (alloy:extent-h target) (alloy:extent-h extent))
        (setf (clip-mask transform) (alloy:copy-extent extent)))))

(defmethod clip ((transform transform) (none null))
  (setf (clip-mask transform) NIL))

(defmethod translate ((transform transform) (point alloy:point))
  (add-matrix transform (matrix 1 0 (alloy:point-x point)
                                0 1 (alloy:point-y point)
                                0 0 1)))

(defmethod scale ((transform transform) (size size))
  (add-matrix transform (matrix (alloy:size-w size) 0 0
                                0 (alloy:size-h size) 0
                                0 0 1)))

(defmethod rotate ((transform transform) (phi float))
  (add-matrix transform (matrix (cos phi) (- (sin phi)) 0
                                (sin phi) (cos phi) 0
                                0 0 1)))

(defclass simple-transformed-renderer (simple-renderer)
  ((transform-stack :accessor transform-stack)))

(defmethod initialize-instance :after ((renderer simple-transformed-renderer) &key)
  (setf (transform-stack renderer) (list (make-default-transform renderer))))

(defgeneric make-default-transform (renderer))

(defmethod make-default-transform ((renderer simple-transformed-renderer))
  (make-instance 'transform
                 :clip-mask NIL
                 :transform-matrix (matrix-identity)))

(defmethod push-transforms ((renderer simple-transformed-renderer))
  (let ((current (car (transform-stack renderer))))
    (push (make-instance 'transform :parent current)
          (transform-stack renderer))))

(defmethod pop-transforms ((renderer simple-transformed-renderer))
  (pop (transform-stack renderer))
  (unless (transform-stack renderer)
    (setf (transform-stack renderer) (list (make-default-transform renderer)))))

(defmethod clip ((renderer simple-transformed-renderer) region)
  (clip (car (transform-stack renderer)) region))

(defmethod translate ((renderer simple-transformed-renderer) point)
  (translate (car (transform-stack renderer)) point))

(defmethod scale ((renderer simple-transformed-renderer) size)
  (scale (car (transform-stack renderer)) size))

(defmethod rotate ((renderer simple-transformed-renderer) phi)
  (rotate (car (transform-stack renderer)) phi))
