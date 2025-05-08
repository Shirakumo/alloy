(in-package #:org.shirakumo.alloy.animation)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *easings* (make-hash-table :test 'eql))

  (defun easing (name &optional (errorp T))
    (or (gethash name *easings*)
        (when errorp (error "No easing function named ~s known." name))))

  (define-compiler-macro easing (&whole whole &environment env name &optional (errorp T))
    (if (constantp name env)
        `(load-time-value (or (gethash ,name *easings*)
                              (when ,errorp (error "No easing function named ~s known." ,name))))
        whole))

  (defun (setf easing) (func name)
    (setf (gethash name *easings*) func)))

(defun ease (by x)
  (let ((easing (easing by)))
    (declare (type function easing))
    (funcall easing (float x 0f0))))

(define-compiler-macro ease (by x &environment env)
  (if (constantp by env)
      `(funcall (the (function (single-float) single-float) (load-time-value (easing ,by))) (float ,x 0f0))
      `(funcall (the (function (single-float) single-float) (easing ,by)) (float ,x 0f0))))

(defmacro define-easing (name (x) &body body)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf (easing ',name)
           (lambda (,x)
             (declare (type (single-float 0f0) ,x))
             (declare (optimize speed))
             ,@body))
     (setf (easing ',(intern (string name) "KEYWORD")) (easing ',name))))

(define-easing linear (x)
  x)

(define-easing quad-in (x)
  (expt x 2))

(define-easing quad-out (x)
  (- (* x (- x 2))))

(define-easing quad-in-out (x)
  (let ((x (* x 2)))
    (if (< x 1)
        (/ (expt x 2) 2)
        (- (/ (1- (* (decf x) (- x 2))) 2)))))

(define-easing cubic-in (x)
  (expt x 3))

(define-easing cubic-out (x)
  (1+ (expt (1- x) 3)))

(define-easing cubic-in-out (x)
  (let ((x (* x 2)))
    (if (< x 1)
        (/ (expt x 3) 2)
        (/ (+ (expt (- x 2) 3) 2) 2))))

(define-easing quart-in (x)
  (expt x 4))

(define-easing quart-out (x)
  (- (1- (expt (1- x) 4))))

(define-easing quart-in-out (x)
  (let ((x (* x 2)))
    (if (< x 1)
        (/ (expt x 4) 2)
        (- (/ (- (expt (- x 2) 4) 2) 2)))))

(define-easing quint-in (x)
  (expt x 5))

(define-easing quint-out (x)
  (1+ (expt (1- x) 5)))

(define-easing quint-in-out (x)
  (let ((x (* x 2)))
    (if (< x 1)
        (/ (expt x 5) 2)
        (/ (+ (expt (- x 2) 5) 2) 2))))

(define-easing sine-in (x)
  (1+ (- (float (cos (* x (/ PI 2))) 0f0))))

(define-easing sine-out (x)
  (float (sin (* x (/ PI 2))) 0f0))

(define-easing sine-in-out (x)
  (- (/ (1- (float (cos (* PI x)) 0f0)) 2)))

(define-easing expo-in (x)
  (if (= 0 x)
      0
      (expt 2 (* 10 (1- x)))))

(define-easing expo-out (x)
  (if (= 1 x)
      1
      (1+ (- (expt 2 (* x -10))))))

(define-easing expo-in-out (x)
  (case x
    ((1 0) x)
    (T (let ((x (* x 2)))
         (if (< x 1)
             (/ (expt 2 (* 10 (1- x))) 2)
             (/ (+ (- (expt 2 (* x -10))) 2) 2))))))

(define-easing circ-in (x)
  (- (1- (sqrt (- 1 (expt x 2))))))

(define-easing circ-out (x)
  (sqrt (- 1 (expt (1- x) 2))))

(define-easing circ-in-out (x)
  (let ((x (* x 2)))
    (if (< x 1)
        (- (/ (1- (sqrt (- 1 (expt x 2)))) 2))
        (/ (1+ (sqrt (- 1 (expt x 2)))) 2))))

(define-easing back-in (x)
  (let ((s 1.70158))
    (* (expt x 2) (- (* (1+ s) x) s))))

(define-easing back-out (x)
  (let ((s 1.70158))
    (1+ (* (expt x 2) (+ (* (1+ s) x) s)))))

(define-easing back-in-out (x)
  (let ((s (* 1.70158 1.525))
        (x (* x 2)))
    (if (< x 1)
        (/ (* (expt x 2) (- (* (1+ s) x) s)) 2)
        (/ (+ (* (expt x 2) (+ (* (1+ s) x) s)) 2) 2))))

(define-easing elastic-in (x)
  (case x
    ((0 1) x)
    (T (let* ((p 0.3)
              (s (/ p 4)))
         (- (* (expt 2f0 (* x 10)) (float (sin (/ (* (- x s) 2 PI) p)) 0f0)))))))

(define-easing elastic-out (x)
  (case x
    ((0 1) x)
    (T (let* ((p 0.3)
              (s (/ p 4)))
         (1+ (* (expt 2f0 (* x -10)) (float (sin (/ (* (- x s) 2 PI) p)) 0f0)))))))

(define-easing elastic-in-out (x)
  (case x
    ((0 1) x)
    (T (let* ((x (* x 2))
              (p (* 0.3 1.5))
              (s (/ p 4)))
         (if (< x 1)
             (- (/ (* (expt 2 (* (1- x) 10)) (float (sin (/ (* (- (1- x) s) 2 PI) p)) 0f0)) 2))
             (1+ (/ (* (expt 2 (* x -10)) (float (sin (/ (* (- (1- x) s) 2 PI) p)) 0f0)) 2)))))))

(define-easing bounce-out (x)
  (let ((s 7.5625)
        (p 2.75))
    (cond ((< x (/ 1 p))
           (* s (expt x 2)))
          ((< x (/ 2 p))
           (+ (* s (expt (- x (/ 1.5 p)) 2)) 0.75))
          ((< x (/ 2.5 p))
           (+ (* s (expt (- x (/ 2.25 p)) 2)) 0.9375))
          (T
           (+ (* s (expt (- x (/ 2.625 p)) 2)) 0.984375)))))

(define-easing bounce-in (x)
  (- 1 (ease 'bounce-out (- 1 x))))

(define-easing bounce-in-out (x)
  (if (< x 0.5)
      (/ (ease 'bounce-in (* x 2)) 2)
      (/ (1+ (ease 'bounce-out (1- (* x 2)))) 2)))
