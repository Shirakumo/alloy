#|
 This file is a part of Alloy
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.alloy)

(defgeneric x (geometry))
(defgeneric y (geometry))
(defgeneric w (geometry))
(defgeneric h (geometry))
(defgeneric l (geometry))
(defgeneric u (geometry))
(defgeneric r (geometry))
(defgeneric b (geometry))
(defgeneric contained-p (point extent))
(defun pxx (geometry) (to-px (x geometry)))
(defun pxy (geometry) (to-px (y geometry)))
(defun pxw (geometry) (to-px (w geometry)))
(defun pxh (geometry) (to-px (h geometry)))
(defun pxl (geometry) (to-px (l geometry)))
(defun pxu (geometry) (to-px (u geometry)))
(defun pxr (geometry) (to-px (r geometry)))
(defun pxb (geometry) (to-px (b geometry)))

(declaim (inline %point))
(defstruct (point (:constructor %point (x y))
                  (:copier NIL))
  (x NIL :type unit)
  (y NIL :type unit))

(defmethod print-object ((point point) stream)
  (format stream "~s" (list 'point (point-x point) (point-y point))))

(defmethod make-load-form ((point point) &optional env)
  (declare (ignore env))
  (list '%point (point-x point) (point-y point)))

(defun point (&optional (x 0) (y 0))
  (%point (unit x) (unit y)))

(define-compiler-macro point (&whole whole &optional (x 0) (y 0) &environment env)
  (if (and (constantp x env) (constantp y env))
      `(load-time-value (%point (unit ,x) (unit ,y)))
      whole))

(defun px-point (&optional (x 0) (y 0))
  (%point (px x) (px y)))

(defmethod x ((point point)) (point-x point))
(defmethod y ((point point)) (point-y point))
(defmethod l ((point point)) (point-x point))
(defmethod b ((point point)) (point-y point))

(defun point= (a b)
  (and (u= (point-x a) (point-x b))
       (u= (point-y a) (point-y b))))

(declaim (inline %size))
(defstruct (size (:constructor %size (w h))
                 (:copier NIL))
  (w NIL :type unit)
  (h NIL :type unit))

(defmethod print-object ((size size) stream)
  (format stream "~s" (list 'size (size-w size) (size-h size))))

(defmethod make-load-form ((size size) &optional env)
  (declare (ignore env))
  (list '%size (size-w size) (size-h size)))

(defun size (&optional w h)
  (cond (h (%size (unit w) (unit h)))
        (w (%size (unit w) (unit w)))
        (T (%size (unit 0) (unit 0)))))

(define-compiler-macro size (&whole whole &optional (w 0 w-p) (h 0 h-p) &environment env)
  (cond (h-p
         (if (and (constantp w env) (constantp h env))
             `(load-time-value (%size (unit ,w) (unit ,h)))
             whole))
        (w-p
         (if (constantp w env)
             `(load-time-value (%size (unit ,w) (unit ,w)))
             whole))
        (T
         `(load-time-value (%size (unit 0) (unit 0))))))

(defun px-size (&optional w h)
  (cond (h (%size (px w) (px h)))
        (w (%size (px w) (px w)))
        (T (%size (px 0) (px 0)))))

(defmethod w ((size size)) (size-w size))
(defmethod h ((size size)) (size-h size))

(defun size= (a b)
  (and (u= (size-w a) (size-w b))
       (u= (size-h a) (size-h b))))

(declaim (inline %margins))
(defstruct (margins (:constructor %margins (l u r b))
                    (:copier NIL))
  (l NIL :type unit)
  (u NIL :type unit)
  (r NIL :type unit)
  (b NIL :type unit))

(defmethod print-object ((margins margins) stream)
  (format stream "~s" (list 'margins (margins-l margins) (margins-u margins) (margins-r margins) (margins-b margins))))

(defmethod make-load-form ((margins margins) &optional env)
  (declare (ignore env))
  (list '%margins (margins-l margins) (margins-u margins) (margins-r margins) (margins-b margins)))

(defun margins (&optional l u r b)
  (cond (b (%margins (unit l) (unit u) (unit r) (unit b)))
        (r (%margins (unit l) (unit u) (unit r) (unit 0)))
        (u (%margins (unit l) (unit u) (unit l) (unit u)))
        (l (%margins (unit l) (unit l) (unit l) (unit l)))
        (T (%margins (unit 0) (unit 0) (unit 0) (unit 0)))))

(define-compiler-macro margins (&whole whole &optional (l 0 l-p) (u 0 u-p) (r 0 r-p) (b 0 b-p) &environment env)
  (cond (b-p
         (if (and (constantp l env) (constantp u env) (constantp r env) (constantp b env))
             `(load-time-value (%margins (unit ,l) (unit ,u) (unit ,r) (unit ,b)))
             whole))
        (r-p
         (if (and (constantp l env) (constantp u env) (constantp r env))
             `(load-time-value (%margins (unit ,l) (unit ,u) (unit ,r) (unit 0)))
             whole))
        (u-p
         (if (and (constantp l env) (constantp u env))
             `(load-time-value (%margins (unit ,l) (unit ,u) (unit ,l) (unit ,u)))
             whole))
        (l-p
         (if (and (constantp l env))
             `(load-time-value (%margins (unit ,l) (unit ,l) (unit ,l) (unit ,l)))
             whole))
        (T
         `(load-time-value (%margins (unit 0) (unit 0) (unit 0) (unit 0))))))

(defun px-margins (&optional l u r b)
  (cond (b (%margins (px l) (px u) (px r) (px b)))
        (r (%margins (px l) (px u) (px r) (px 0)))
        (u (%margins (px l) (px u) (px l) (px u)))
        (l (%margins (px l) (px l) (px l) (px l)))
        (T (%margins (px 0) (px 0) (px 0) (px 0)))))

(defmethod l ((margins margins)) (margins-l margins))
(defmethod u ((margins margins)) (margins-u margins))
(defmethod r ((margins margins)) (margins-r margins))
(defmethod b ((margins margins)) (margins-b margins))

(defun margins= (a b)
  (and (u= (margins-l a) (margins-l b))
       (u= (margins-u a) (margins-u b))
       (u= (margins-r a) (margins-r b))
       (u= (margins-b a) (margins-b b))))

(defmacro destructure-margins ((&rest args &key l u r b to-px) margins &body body)
  (declare (ignore l u r b))
  (let ((marginsg (gensym "EXTENT")))
    `(let* ((,marginsg ,margins)
            ,@(loop for (name func) in '((:l margins-l) (:u margins-u) (:r margins-r) (:b margins-b))
                    for var = (getf args name)
                    when var
                    collect `(,var (,(if to-px 'to-px 'identity) (,func ,marginsg)))))
       ,@body)))

(declaim (inline %extent))
(defstruct (extent (:include size)
                   (:constructor %extent (x y w h)))
  (x NIL :type unit)
  (y NIL :type unit))

(defmethod print-object ((extent extent) stream)
  (format stream "~s" (list 'extent (extent-x extent) (extent-y extent) (extent-w extent) (extent-h extent))))

(defmethod make-load-form ((extent extent) &optional env)
  (declare (ignore env))
  (list '%extent (extent-x extent) (extent-y extent) (extent-w extent) (extent-h extent)))

(defun extent (&optional (x 0) (y 0) (w 0) (h 0))
  (%extent (unit x) (unit y) (unit w) (unit h)))

(define-compiler-macro extent (&whole whole &optional (x 0) (y 0) (w 0) (h 0) &environment env)
  (if (and (constantp x env) (constantp y env) (constantp w env) (constantp h env))
      `(load-time-value (%extent (unit ,x) (unit ,y) (unit ,w) (unit ,h)))
      whole))

(defun px-extent (&optional (x 0) (y 0) (w 0) (h 0))
  (%extent (px x) (px y) (px w) (px h)))

(defun extent= (a b)
  (and (u= (extent-x a) (extent-x b))
       (u= (extent-y a) (extent-y b))
       (u= (extent-w a) (extent-w b))
       (u= (extent-h a) (extent-h b))))

(defmethod x ((extent extent)) (extent-x extent))
(defmethod y ((extent extent)) (extent-y extent))
(defmethod w ((extent extent)) (extent-w extent))
(defmethod h ((extent extent)) (extent-h extent))
(defmethod l ((extent extent)) (extent-x extent))
(defmethod b ((extent extent)) (extent-y extent))

(defmethod contained-p ((point point) (extent extent))
  (and (u<= 0 (u- (point-x point) (extent-x extent)) (extent-w extent))
       (u<= 0 (u- (point-y point) (extent-y extent)) (extent-h extent))))

(defmethod contained-p ((inner extent) (outer extent))
  (and (u<= 0 (u- (extent-x inner) (extent-x outer)) (u- (extent-w outer) (extent-w inner)))
       (u<= 0 (u- (extent-y inner) (extent-y outer)) (u- (extent-h outer) (extent-h inner)))))

(defmacro destructure-extent ((&rest args &key x y w h to-px) extent &body body)
  (declare (ignore x y w h))
  (let ((extentg (gensym "EXTENT")))
    `(let* ((,extentg ,extent)
            ,@(loop for (name func) in '((:x extent-x) (:y extent-y) (:w extent-w) (:h extent-h))
                    for var = (getf args name)
                    when var
                    collect `(,var (,(if to-px 'to-px 'identity) (,func ,extentg)))))
       ,@body)))

(defun extent-intersection (a b)
  (destructure-extent (:x x1 :y y1 :w w1 :h h1 :to-px T) a
    (destructure-extent (:x x2 :y y2 :w w2 :h h2 :to-px T) b
      (let* ((r1 (+ x1 w1)) (u1 (+ y1 h1))
             (r2 (+ x2 w2)) (u2 (+ y2 h2))
             (x (max x1 x2))
             (y (max y1 y2))
             (w (- (min r1 r2) x))
             (h (- (min u1 u2) y)))
        (cond ((and (<= w 0) (<= h 0))
               (load-time-value (px-extent 0 0 0 0)))
              ((and (= x x1) (= y y1) (= w w1) (= h h1))
               a)
              ((and (= x x2) (= y y2) (= w w2) (= h h2))
               b)
              (T
               (px-extent x y w h)))))))

(defun overlapping-p (a b)
  (destructure-extent (:x x1 :y y1 :w w1 :h h1 :to-px T) a
    (destructure-extent (:x x2 :y y2 :w w2 :h h2 :to-px T) b
      (and (< x1 (+ x2 w2)) (< x2 (+ x1 w1))
           (< y1 (+ y2 h2)) (< y2 (+ y1 h1))))))

(defun widen (extent margins)
  (extent (u- (x extent) (l margins))
          (u- (y extent) (b margins))
          (u+ (w extent) (l margins) (r margins))
          (u+ (h extent) (b margins) (u margins))))

(defun ensure-extent (extent-ish &optional base)
  (etypecase extent-ish
    (extent
     extent-ish)
    (margins
     (if base
         (px-extent
          (+ (pxx base) (pxl extent-ish))
          (+ (pxy base) (pxb extent-ish))
          (- (pxw base) (pxr extent-ish) (pxl extent-ish))
          (- (pxh base) (pxu extent-ish) (pxb extent-ish)))
         (px-extent
          (pxl extent-ish) (pxb extent-ish)
          (- (pxw *unit-parent*) (pxr extent-ish) (pxl extent-ish))
          (- (pxh *unit-parent*) (pxu extent-ish) (pxb extent-ish)))))
    (size
     (if base
         (extent (x base) (y base) (w extent-ish) (h extent-ish))
         (extent 0 0 (w extent-ish) (h extent-ish))))
    (point
     (if base
         (extent (x extent-ish) (y extent-ish) (w base) (h base))
         (extent (x extent-ish) (y extent-ish) 0 0)))))
