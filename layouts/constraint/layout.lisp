#|
 This file is a part of Alloy
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.alloy.layouts.constraint)

(defun make-variables (element solver &key (strength :medium))
  (list (cass:make-variable solver :name (format NIL "X ~a" element) :strength strength)
        (cass:make-variable solver :name (format NIL "Y ~a" element) :strength strength)
        (cass:make-variable solver :name (format NIL "W ~a" element) :strength strength)
        (cass:make-variable solver :name (format NIL "H ~a" element) :strength strength)))

(defclass layout (alloy:layout alloy:layout-element alloy:vector-container)
  ((solver :initform (cass:make-solver) :reader solver)
   (variables :initform (make-hash-table :test 'eq) :reader variables)
   (constraints :initform (make-hash-table :test 'eq) :reader constraints)))

(defmethod initialize-instance :after ((layout layout) &key)
  (setf (gethash layout (variables layout))
        (make-variables layout (solver layout) :strength :strong)))

(defmacro with-vars ((x y w h layout) element &body body)
  `(destructuring-bind (,x ,y ,w ,h)
       (gethash ,element (variables ,layout))
     ,@body))

(defun suggest (layout element extent)
  (with-vars (x y w h layout) element
    (cass:suggest x (alloy:extent-x extent))
    (cass:suggest y (alloy:extent-x extent))
    (cass:suggest w (alloy:extent-w extent))
    (cass:suggest h (alloy:extent-h extent))
    (cass:update-variables (solver layout))))

(defun update (layout element)
  (with-vars (x y w h layout) element
    (print (list element x y w h))
    (setf (alloy:bounds element) (alloy:extent (cass:value x) (cass:value y) (cass:value w) (cass:value h)))))

(defmethod alloy:enter ((element alloy:layout-element) (layout layout) &key constraints)
  (call-next-method)
  (setf (gethash element (variables layout))
        (make-variables element (solver layout)))
  (apply-constraints constraints element layout))

(defmethod alloy:enter ((element alloy:layout-entry) (layout layout) &key)
  (call-next-method)
  (setf (gethash (alloy:component element) (variables layout))
        (gethash element (variables layout))))

(defmethod alloy:leave :after ((element alloy:layout-element) (layout layout))
  (remhash element (variables layout)))

(defmethod alloy:leave :after ((element alloy:layout-entry) (layout layout))
  (remhash (alloy:component element) (variables layout)))

(defmethod alloy:update ((element alloy:layout-element) (layout layout) &key constraints)
  (apply-constraints constraints element layout))

(defmethod alloy:notice-bounds ((element alloy:layout-element) (layout layout)))

(defmethod (setf alloy:bounds) :after (extent (layout layout))
  (suggest layout layout extent)
  (alloy:do-elements (element layout)
    (update layout element)))

(defmethod alloy:suggest-bounds (extent (layout layout))
  (suggest layout layout extent)
  (with-vars (x y w h layout) layout
    (alloy:extent (cass:value x) (cass:value y) (cass:value w) (cass:value h))))

(defun rewrite-variable (var element layout)
  (with-vars (rx ry rw rh layout) layout
    (with-vars (x y w h layout) element
      (case var
        (:x x)
        (:y y)
        (:w w)
        (:h h)
        (:l `(- x rx))
        (:b `(- y ry))
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
         (:left-to
          (r `(= :r (:l ,(second expression)))))
         (:right-to
          (r `(= :l (:r ,(second expression)))))
         (:above
          (r `(<= (+ (:y ,(second expression))
                     (:h ,(second expression)))
                  :y)))
         (:below
          (r `(<= (+ :y :h)
                  (:y ,(second expression)))))
         (:aspect-ratio
          (r `(= :w (* :h ,(second expression)))))
         (:min-width
          (r `(<= ,(second expression) :w)))
         (:min-height
          (r `(<= ,(second expression) :h)))
         (:max-width
          (r `(<= :w ,(second expression))))
         (:max-height
          (r `(<= :h ,(second expression))))
         (:width
          (r `(= :w ,(second expression))))
         (:height
          (r `(= :h ,(second expression))))
         (T
          (list* (first expression)
                 (loop for term in (rest expression)
                       collect (r term)))))))))

(defun apply-constraints (constraints element layout)
  ;; FIXME: how to remove constraints?
  ;; (dolist (constraint (gethash element (constraints layout)))
  ;;   (cass:delete-constraint constraint))
  (flet ((add (expression &key (strength :required))
           (push (cass:constrain-with (solver layout) (rewrite-expression expression element layout) :strength strength)
                 (gethash element (constraints layout)))))
    (dolist (constraint constraints layout)
      (case constraint
        (:square
         (add `(= :w :h)))
        (:contained
         (add `(<= 0 :l))
         (add `(<= 0 :r))
         (add `(<= 0 :u))
         (add `(<= 0 :b)))
        (T (add constraint))))))
