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
        (:l x)
        (:b y)
        (:r `(- ,rw (+ ,x ,w)))
        (:u `(- ,rh (+ ,y ,h)))
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
         ((:x :y :w :h :l :b :r :u :rx :ry :rw :rh)
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

(define-expression-transform :strength (strength &rest expressions)
  (values
   (loop for expression in expressions
         append (transform-expression expression))
   strength))

(define-expression-transform :weak (&rest expressions)
  (values
   (loop for expression in expressions
         append (transform-expression expression))
   :weak))

(define-expression-transform :medium (&rest expressions)
  (values
   (loop for expression in expressions
         append (transform-expression expression))
   :medium))

(define-expression-transform :strong (&rest expressions)
  (values
   (loop for expression in expressions
         append (transform-expression expression))
   :medium))

(define-expression-transform :required (&rest expressions)
  (values
   (loop for expression in expressions
         append (transform-expression expression))
   :required))

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

(define-expression-transform :left-of (other &optional gap)
  (check-type other alloy:layout-element)
  (list (if gap
            `(= (+ :x :w ,gap) (:x ,other))
            `(<= (+ :x :w) (:x ,other)))))

(define-expression-transform :right-of (other &optional gap)
  (check-type other alloy:layout-element)
  (list (if gap
            `(= :x (+ (:x ,other) (:w ,other) ,gap))
            `(>= :x (+ (:x ,other) (:w ,other))))))

(define-expression-transform :above (other &optional gap)
  (check-type other alloy:layout-element)
  (list (if gap
            `(= :y (+ (:y ,other) (:h ,other) ,gap))
            `(>= :y (+ (:y ,other) (:h ,other))))))

(define-expression-transform :below (other &optional gap)
  (check-type other alloy:layout-element)
  (list (if gap
            `(= (+ :y :h ,gap) (:y ,other))
            `(<= (+ :y :h) (:y ,other)))))

(define-expression-transform :chain (dir other &optional (gap 0))
  (check-type other alloy:layout-element)
  (ecase dir
    (:right
     (list `(= :x (+ ,gap (:x ,other) (:w ,other)))
           `(= :h (:h ,other))
           `(= :y (:y ,other))))
    (:left
     (list `(= :x (- (:x ,other) :w ,gap))
           `(= :h (:h ,other))
           `(= :y (:y ,other))))
    ((:down :below)
     (list `(= :x (:x ,other))
           `(= :w (:w ,other))
           `(= :y (- (:y ,other) :h ,gap))))
    ((:up :above)
     (list `(= :x (:x ,other))
           `(= :w (:w ,other))
           `(= :b (+ (:y ,other) (:h ,other) ,gap))))))

(define-expression-transform :inside (other &key (halign :center) (valign :center) (margin 0))
  (check-type other alloy:layout-element)
  (list
   (ecase halign
     (:center
      `(= (- :x (/ :w 2)) (- (:x ,other) (/ (:w ,other) 2))))
     ((:left :start)
      `(= :x (+ (:x ,other) ,margin)))
     ((:right :end)
      `(= (+ :x :w ,margin) (+ (:x ,other) (:w ,other)))))
   (ecase valign
     (:center
      `(= (- :y (/ :h 2)) (- (:y ,other) (/ (:h ,other) 2))))
     ((:bottom :start)
      `(= :y (+ (:y ,other) ,margin)))
     ((:top :end)
      `(= (+ :y :h ,margin) (+ (:y ,other) (:h ,other)))))))

(define-expression-transform :align (edge other &optional (margin 0))
  (check-type other alloy:layout-element)
  (list
   (ecase edge
     ((:north :top)
      `(= (+ (:y ,other) (:h ,other)) (+ :y :h ,margin)))
     ((:east :right)
      `(= (+ (:x ,other) (:w ,other)) (+ :x :w ,margin)))
     ((:south :bottom)
      `(= (:y ,other) (- :y ,margin)))
     ((:west :left)
      `(= (:x ,other) (- :x ,margin))))))

(define-expression-transform :aspect-ratio (ratio)
  (list `(= :w (* :h ,ratio))))

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

(define-expression-transform :fill (&rest what)
  (loop for item in (or what '(:w :h))
        append (ecase item
                 ((:x :w :width) (list `(= :l 0) `(= :r 0)))
                 ((:y :h :height) (list `(= :b 0) `(= :u 0))))))

(define-expression-transform :center (&rest what)
  (loop for item in (or what '(:w :h))
        collect (ecase item
                  ((:x :w :width) `(= :x (- (+ :rx (/ :rw 2)) (/ :w 2))))
                  ((:y :h :height) `(= :y (- (+ :ry (/ :rh 2)) (/ :h 2)))))))
