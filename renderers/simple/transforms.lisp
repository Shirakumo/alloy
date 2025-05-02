(in-package #:org.shirakumo.alloy.renderers.simple)
#.(when (find-package "SB-SIMD-SSE")
    (pushnew :sb-simd-sse *features*))

(deftype matrix ()
  '(simple-array single-float (16)))

(defun matrix (&rest values)
  (let ((matrix (make-array 16 :element-type 'single-float)))
    (map-into matrix (lambda (x) (float x 0f0)) values)))

(define-compiler-macro matrix (&rest values &environment env)
  (flet ((fold (arg)
           (if (constantp arg env)
               `(load-time-value (float ,arg 0f0))
               `(float ,arg 0f0))))
    (let ((matrix (gensym "MATRIX")))
      `(let ((,matrix (make-array 16 :element-type 'single-float)))
         (declare (type matrix ,matrix))
         (declare (optimize speed))
         ,@(loop for value in values
                 for i from 0
                 collect `(setf (aref ,matrix ,i) ,(fold value)))
         ,matrix))))

(declaim (inline copy-matrix))
(defun copy-matrix (matrix)
  (declare (type matrix matrix))
  (make-array 16 :element-type 'single-float :initial-contents matrix))

(declaim (ftype (function () matrix) matrix-identity))
(defun matrix-identity ()
  (matrix 1 0 0 0
          0 1 0 0
          0 0 1 0
          0 0 0 1))

(declaim (inline mref (setf mref)))
(defun mref (mat y x)
  (declare (type matrix mat))
  (declare (type (integer 0 3) x))
  (declare (type (integer 0 3) y))
  (declare (optimize speed (safety 0)))
  (aref mat (+ x (* y 4))))

(defun (setf mref) (value mat y x)
  (declare (type matrix mat))
  (declare (type single-float value))
  (declare (type (integer 0 3) x))
  (declare (type (integer 0 3) y))
  (declare (optimize speed (safety 0)))
  (setf (aref mat (+ x (* y 4))) value))

(defmacro with-matrix ((mat &rest els) &body body)
  `(let ((,mat (make-array 16 :element-type 'single-float)))
     (declare (type matrix ,mat))
     (declare (dynamic-extent ,mat))
     ,@(loop for el in els
             for i from 0
             collect `(setf (aref ,mat ,i) (float ,el 0f0)))
     ,@body))

(declaim (inline mat*p mat*v))
(defun mat*p (mat x y)
  (declare (type matrix mat))
  (declare (type single-float x y))
  (declare (optimize speed (safety 1)))
  (values (+ (* (mref mat 0 0) x) (* (mref mat 0 1) y) (mref mat 0 3))
          (+ (* (mref mat 1 0) x) (* (mref mat 1 1) y) (mref mat 1 3))))

(defun mat*v (mat x y)
  (declare (type matrix mat))
  (declare (type single-float x y))
  (declare (optimize speed (safety 1)))
  (values (+ (* (mref mat 0 0) x) (* (mref mat 0 1) y))
          (+ (* (mref mat 1 0) x) (* (mref mat 1 1) y))))

(defun mat* (r a b)
  (declare (type matrix r a b))
  (declare (optimize speed (safety 1)))
  #-sb-simd-sse
  (let ((a0 (aref a 0)) (a1 (aref a 1)) (a2 (aref a 2)) (a3 (aref a 3))
        (a4 (aref a 4)) (a5 (aref a 5)) (a6 (aref a 6)) (a7 (aref a 7))
        (a8 (aref a 8)) (a9 (aref a 9)) (a10 (aref a 10)) (a11 (aref a 11))
        (a12 (aref a 12)) (a13 (aref a 13)) (a14 (aref a 14)) (a15 (aref a 15))
        (b0 (aref b 0)) (b1 (aref b 1)) (b2 (aref b 2)) (b3 (aref b 3))
        (b4 (aref b 4)) (b5 (aref b 5)) (b6 (aref b 6)) (b7 (aref b 7))
        (b8 (aref b 8)) (b9 (aref b 9)) (b10 (aref b 10)) (b11 (aref b 11))
        (b12 (aref b 12)) (b13 (aref b 13)) (b14 (aref b 14)) (b15 (aref b 15)))
    (setf (aref r 0) (+ (* a0 b0) (* a1 b4) (* a2 b8) (* a3 b12)))
    (setf (aref r 1) (+ (* a0 b1) (* a1 b5) (* a2 b9) (* a3 b13)))
    (setf (aref r 2) (+ (* a0 b2) (* a1 b6) (* a2 b10) (* a3 b14)))
    (setf (aref r 3) (+ (* a0 b3) (* a1 b7) (* a2 b11) (* a3 b15)))
    (setf (aref r 4) (+ (* a4 b0) (* a5 b4) (* a6 b8) (* a7 b12)))
    (setf (aref r 5) (+ (* a4 b1) (* a5 b5) (* a6 b9) (* a7 b13)))
    (setf (aref r 6) (+ (* a4 b2) (* a5 b6) (* a6 b10) (* a7 b14)))
    (setf (aref r 7) (+ (* a4 b3) (* a5 b7) (* a6 b11) (* a7 b15)))
    (setf (aref r 8) (+ (* a8 b0) (* a9 b4) (* a10 b8) (* a11 b12)))
    (setf (aref r 9) (+ (* a8 b1) (* a9 b5) (* a10 b9) (* a11 b13)))
    (setf (aref r 10) (+ (* a8 b2) (* a9 b6) (* a10 b10) (* a11 b14)))
    (setf (aref r 11) (+ (* a8 b3) (* a9 b7) (* a10 b11) (* a11 b15)))
    (setf (aref r 12) (+ (* a12 b0) (* a13 b4) (* a14 b8) (* a15 b12)))
    (setf (aref r 13) (+ (* a12 b1) (* a13 b5) (* a14 b9) (* a15 b13)))
    (setf (aref r 14) (+ (* a12 b2) (* a13 b6) (* a14 b10) (* a15 b14)))
    (setf (aref r 15) (+ (* a12 b3) (* a13 b7) (* a14 b11) (* a15 b15))))
  #+sb-simd-sse
  (let ((b0 (sb-simd-sse:f32.4-row-major-aref b 0))
        (b1 (sb-simd-sse:f32.4-row-major-aref b 4))
        (b2 (sb-simd-sse:f32.4-row-major-aref b 8))
        (b3 (sb-simd-sse:make-f32.4 0.0 0.0 0.0 1.0)))
    (loop for i from 0 below 16 by 4
          do (let ((br0 (sb-simd-sse:f32.4-broadcast (aref a (+ i 0))))
                   (br1 (sb-simd-sse:f32.4-broadcast (aref a (+ i 1))))
                   (br2 (sb-simd-sse:f32.4-broadcast (aref a (+ i 2))))
                   (br3 (sb-simd-sse:f32.4-broadcast (aref a (+ i 3)))))
               (setf (sb-simd-sse:f32.4-row-major-aref r i)
                     (sb-simd-sse:f32.4+
                      (sb-simd-sse:f32.4* br0 b0)
                      (sb-simd-sse:f32.4* br1 b1)
                      (sb-simd-sse:f32.4* br2 b2)
                      (sb-simd-sse:f32.4* br3 b3))))))
  r)

(defclass transformed-renderer (renderer)
  ((transform-matrix :initform (matrix-identity) :accessor transform-matrix)
   (identity-matrix :initform (matrix-identity) :accessor identity-matrix)))

(defmethod call-with-pushed-transforms (function (renderer transformed-renderer) &key clear)
  (let ((current (transform-matrix renderer))
        (new (make-array 16 :element-type 'single-float)))
    (declare (dynamic-extent new))
    (declare (type matrix current new))
    (declare (optimize speed))
    (if clear
        (dotimes (i 16) (setf (aref new i) (aref (the matrix (identity-matrix renderer)) i)))
        (dotimes (i 16) (setf (aref new i) (aref current i))))
    (setf (transform-matrix renderer) new)
    (unwind-protect
         (funcall (the function function))
      (setf (transform-matrix renderer) current))))

(defmethod add-matrix ((renderer transformed-renderer) new)
  (let ((ex (transform-matrix renderer)))
    (setf (transform-matrix renderer) (mat* ex ex new))
    renderer))

(defun translate-by (renderer pxx pxy)
  (let* ((matrix (transform-matrix renderer)))
    (incf (mref matrix 0 3) (+ (* (mref matrix 0 0) pxx)
                               (* (mref matrix 0 1) pxy)))
    (incf (mref matrix 1 3) (+ (* (mref matrix 1 0) pxx)
                               (* (mref matrix 1 1) pxy)))
    (incf (mref matrix 2 3) (+ (* (mref matrix 2 0) pxx)
                               (* (mref matrix 2 1) pxy)))
    renderer))

(defun scale-by (renderer pxw pxh)
  (let* ((matrix (transform-matrix renderer)))
    (setf (mref matrix 0 0) (* (mref matrix 0 0) pxw))
    (setf (mref matrix 0 1) (* (mref matrix 0 1) pxh))
    (setf (mref matrix 1 0) (* (mref matrix 1 0) pxw))
    (setf (mref matrix 1 1) (* (mref matrix 1 1) pxh))
    (setf (mref matrix 2 0) (* (mref matrix 2 0) pxw))
    (setf (mref matrix 2 1) (* (mref matrix 2 1) pxh))
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
    (with-matrix (mat cos (- sin) 0 0
                      sin    cos  0 0
                        0      0  1 0
                        0      0  0 1)
      (add-matrix renderer mat))))

(defmethod z-index ((renderer transformed-renderer))
  (aref (transform-matrix renderer) 11))

(defmethod (setf z-index) (z-index (renderer transformed-renderer))
  (setf (aref (transform-matrix renderer) 11) (float z-index)))
