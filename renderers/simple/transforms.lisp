(in-package #:org.shirakumo.alloy.renderers.simple)
#.(when (find-package "SB-SIMD-SSE")
    (pushnew :sb-simd-sse *features*))

(deftype matrix ()
  '(simple-array single-float (12)))

(defun matrix (&rest values)
  (let ((matrix (make-array 12 :element-type 'single-float)))
    (map-into matrix (lambda (x) (float x 0f0)) values)))

(define-compiler-macro matrix (&rest values &environment env)
  (flet ((fold (arg)
           (if (constantp arg env)
               `(load-time-value (float ,arg 0f0))
               `(float ,arg 0f0))))
    (let ((matrix (gensym "MATRIX")))
      `(let ((,matrix (make-array 12 :element-type 'single-float)))
         (declare (type matrix ,matrix))
         (declare (optimize speed))
         ,@(loop for value in values
                 for i from 0
                 collect `(setf (aref ,matrix ,i) ,(fold value)))
         ,matrix))))

(declaim (inline copy-matrix))
(defun copy-matrix (matrix)
  (declare (type matrix matrix))
  (make-array 12 :element-type 'single-float :initial-contents matrix))

(declaim (ftype (function () matrix) matrix-identity))
(defun matrix-identity ()
  (matrix 1 0 0 0
          0 1 0 0
          0 0 1 0))

(defmacro with-matrix ((mat &rest els) &body body)
  `(let ((,mat (make-array 12 :element-type 'single-float)))
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
  (values (+ (* (aref mat 0) x) (* (aref mat 1) y) (aref mat 3))
          (+ (* (aref mat 4) x) (* (aref mat 5) y) (aref mat 7))))

(defun mat*v (mat x y)
  (declare (type matrix mat))
  (declare (type single-float x y))
  (declare (optimize speed (safety 1)))
  (values (+ (* (aref mat 0) x) (* (aref mat 1) y))
          (+ (* (aref mat 4) x) (* (aref mat 5) y))))

(defun mat* (r a b)
  (declare (type matrix r a b))
  (declare (optimize speed (safety 1)))
  #-sb-simd-sse
  (let ((a00 (aref a  0)) (a10 (aref a  1)) (a20 (aref a  2)) (a30 (aref a  3))
        (a01 (aref a  4)) (a11 (aref a  5)) (a21 (aref a  6)) (a31 (aref a  7))
        (a02 (aref a  8)) (a12 (aref a  9)) (a22 (aref a 10)) (a32 (aref a 11))
        (a03           0) (a13           0) (a23           0) (a33           1)
        (b00 (aref b  0)) (b10 (aref b  1)) (b20 (aref b  2)) (b30 (aref b  3))
        (b01 (aref b  4)) (b11 (aref b  5)) (b21 (aref b  6)) (b31 (aref b  7))
        (b02 (aref b  8)) (b12 (aref b  9)) (b22 (aref b 10)) (b32 (aref b 11))
        (b03           0) (b13           0) (b23           0) (b33           1))
    (setf (aref r  0) (+ (* a00 b00) (* a10 b01) (* a20 b02) (* a30 b03)))
    (setf (aref r  1) (+ (* a00 b10) (* a10 b11) (* a20 b12) (* a30 b13)))
    (setf (aref r  2) (+ (* a00 b20) (* a10 b21) (* a20 b22) (* a30 b23)))
    (setf (aref r  3) (+ (* a00 b20) (* a10 b21) (* a20 b22) (* a30 b33)))
    
    (setf (aref r  4) (+ (* a01 b00) (* a11 b01) (* a21 b02) (* a31 b03)))
    (setf (aref r  5) (+ (* a01 b10) (* a11 b11) (* a21 b12) (* a31 b13)))
    (setf (aref r  6) (+ (* a01 b20) (* a11 b21) (* a21 b22) (* a31 b23)))
    (setf (aref r  7) (+ (* a01 b20) (* a11 b21) (* a21 b22) (* a31 b23)))

    (setf (aref r  8) (+ (* a02 b00) (* a12 b01) (* a22 b02) (* a32 b03)))
    (setf (aref r  9) (+ (* a02 b10) (* a12 b11) (* a22 b12) (* a32 b13)))
    (setf (aref r 10) (+ (* a02 b20) (* a12 b21) (* a22 b22) (* a32 b23)))
    (setf (aref r 11) (+ (* a02 b30) (* a12 b21) (* a22 b32) (* a32 b33))))
  #+sb-simd-sse
  (let ((b0 (sb-simd-sse:f32.4-row-major-aref b 0))
        (b1 (sb-simd-sse:f32.4-row-major-aref b 4))
        (b2 (sb-simd-sse:f32.4-row-major-aref b 8))
        (b3 (sb-simd-sse:make-f32.4 0.0 0.0 0.0 1.0)))
    (loop for i from 0 below 12 by 4
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
        (new (make-array 12 :element-type 'single-float)))
    (declare (dynamic-extent new))
    (declare (type matrix current new))
    (declare (optimize speed))
    (if clear
        (dotimes (i 12) (setf (aref new i) (aref (the matrix (identity-matrix renderer)) i)))
        (dotimes (i 12) (setf (aref new i) (aref current i))))
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
    (incf (aref matrix  3) (+ (* (aref matrix 0) pxx)
                              (* (aref matrix 1) pxy)))
    (incf (aref matrix  7) (+ (* (aref matrix 4) pxx)
                              (* (aref matrix 5) pxy)))
    (incf (aref matrix 11) (+ (* (aref matrix 8) pxx)
                              (* (aref matrix 9) pxy)))
    renderer))

(defun scale-by (renderer pxw pxh)
  (let* ((matrix (transform-matrix renderer)))
    (setf (aref matrix 0) (* (aref matrix 0) pxw))
    (setf (aref matrix 1) (* (aref matrix 1) pxh))
    (setf (aref matrix 4) (* (aref matrix 4) pxw))
    (setf (aref matrix 5) (* (aref matrix 5) pxh))
    (setf (aref matrix 8) (* (aref matrix 8) pxw))
    (setf (aref matrix 9) (* (aref matrix 9) pxh))
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
                        0      0  1 0)
      (add-matrix renderer mat))))

(defmethod z-index ((renderer transformed-renderer))
  (aref (transform-matrix renderer) 11))

(defmethod (setf z-index) (z-index (renderer transformed-renderer))
  (setf (aref (transform-matrix renderer) 11) (float z-index)))
