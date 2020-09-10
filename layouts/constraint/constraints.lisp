#|
 This file is a part of Alloy
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.alloy.layouts.constraint)

(defvar *expression-transforms* (make-hash-table :test 'eq))

(defmacro define-expression-transform (name args &body body)
  `(setf (gethash ',name *expression-transforms*)
         (lambda ,args
           ,@body)))

(defmacro with-vars ((x y w h layout) element &body body)
  `(destructuring-bind (,x ,y ,w ,h)
       (gethash ,element (variables ,layout))
     ,@body))

(defun rewrite-variable (var element layout)
  (with-vars (rx ry rw rh layout) layout
    (with-vars (x y w h layout) element
      (case var
        (:x x)
        (:y y)
        (:w w)
        (:h h)
        (:l `(- ,x ,rx))
        (:b `(- ,y ,ry))
        (:r `(- (+ ,rx ,rw) (+ ,x ,w)))
        (:u `(- (+ ,ry ,rh) (+ ,y ,h)))
        (:rx rx)
        (:ry ry)
        (:rw rw)
        (:rh rh)
        (T var)))))

(defun rewrite-expression (expression element layout)
  (etypecase expression
    ((or real cass:variable) expression)
    (symbol (rewrite-variable expression element layout))
    (cons
     (flet ((r (expr)
              (rewrite-expression expr element layout)))
       (case (first expression)
         ((:x :y :w :h :l :b :r :u)
          (rewrite-variable (first expression) (second expression) layout))
         (T
          (list* (first expression)
                 (loop for term in (rest expression)
                       collect (r term)))))))))

(defun transform-expression (expression)
  (typecase expression
    (symbol
     (let ((function (gethash expression *expression-transforms*)))
       (if function
           (funcall function)
           (error "Unknown expression ~s" expression))))
    (cons
     (let ((function (gethash (first expression) *expression-transforms*)))
       (if function
           (apply function (rest expression))
           (list expression))))))

(define-expression-transform :center-x ()
  (list `(= (/ :rw 2) (- :l (/ :w 2)))))

(define-expression-transform :center-y ()
  (list `(= (/ :rh 2) (- :b (/ :h 2)))))

(define-expression-transform :left (&optional (un 0))
  (list `(= :l ,un)))

(define-expression-transform :right (&optional (un 0))
  (list `(= :r ,un)))

(define-expression-transform :top (&optional (un 0))
  (list `(= :u ,un)))

(define-expression-transform :bottom (&optional (un 0))
  (list `(= :b ,un)))

(define-expression-transform :width (un)
  (list `(= :w ,un)))

(define-expression-transform :height (un)
  (list `(= :h ,un)))

(define-expression-transform :size (w &optional (h w))
  (list `(= :w ,w)
        `(= :h ,h)))

(define-expression-transform :square ()
  (list `(= :w :h)))

(define-expression-transform :contained ()
  (list `(<= 0 :l)
        `(<= 0 :r)
        `(<= 0 :u)
        `(<= 0 :b)))

(define-expression-transform :margin (&optional (l 0) u r b)
  (let ((b (or b u l))
        (r (or r l))
        (u (or u l)))
    (list `(= ,l :l)
          `(= ,r :r)
          `(= ,u :u)
          `(= ,b :b))))

(define-expression-transform :left-to (other &optional (gap 0))
  (list `(= :r (+ (:l ,other) ,gap))))

(define-expression-transform :right-to (other &optional (gap 0))
  (list `(= :l (+ (:r ,other) ,gap))))

(define-expression-transform :above (other &optional (gap 0))
  (list `(<= (+ (:y ,other) (:h ,other) ,gap) :y)))

(define-expression-transform :below (other &optional (gap 0))
  (list `(<= (+ :y :h) (:y ,other) ,gap)))

(define-expression-transform :aspect-ratio (ratio)
  (list `(= :h (* :w ,ratio))))

(define-expression-transform :min-width (width)
  (list `(<= ,width :w)))

(define-expression-transform :min-height (height)
  (list `(<= ,height :h)))

(define-expression-transform :min-size (width height)
  (list `(<= ,width :w)
        `(<= ,height :h)))

(define-expression-transform :max-width (width)
  (list `(<= :w ,width)))

(define-expression-transform :max-height (height)
  (list `(<= :h ,height)))

(define-expression-transform :max-size (width height)
  (list `(<= ,width :w)
        `(<= ,height :h)))

(define-expression-transform :between-x (left right)
  (list `(<= (:x ,left) :x)
        `(<= (+ :x :w) (:x ,right))))

(define-expression-transform :between-y (left right)
  (list `(<= (:y ,left) :y)
        `(<= (+ :y :h) (:y ,right))))
