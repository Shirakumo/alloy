#|
 This file is a part of Alloy
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.alloy)

(defvar *var-counter*)

(defun next-var (&optional (count (incf *var-counter*)))
  (intern (format NIL "$~a" count) *package*))

(defgeneric build (stub body))

(defmethod build ((stub symbol) body)
  (build (find-class stub) body))

(defmethod build ((class class) body)
  (build (allocate-instance class) body))

(defmacro build-ui (layout focus &rest body)
  (let ((*var-counter* 0))
    (multiple-value-bind (constructor bindings) (build (car layout) (cdr layout))
      `(let ,bindings
         (let ((layout ,constructor)
               (focus ,(build (car focus) (cdr focus))))
           (declare (ignorable layout focus))
           ,@body)))))

(defmacro define-builder (class destructure &body body)
  (let ((expr (gensym "EXPR")))
    `(defmethod build ((,class ,class) ,expr)
       (declare (ignorable ,class))
       (destructuring-bind ,destructure ,expr
         ,@body))))

(defun build-inner (inner)
  (typecase inner
    (atom inner)
    (cons (build (car inner) (cdr inner)))))

(define-builder component (place &rest args)
  (let ((var (next-var)))
    (values var `((,var (represent ,place ',(type-of component) ,@args))))))

(define-builder container (&rest args)
  (let ((body args)
        (kargs ())
        (symb (gensym "CONTAINER"))
        (bindings ()))
    (loop for (name val) = body
          while (keywordp name)
          do (push name kargs)
             (push val kargs)
             (setf body (cddr body)))
    (values
     `(let ((,symb (make-instance ',(type-of container) ,@(nreverse kargs))))
        ,@(loop for expr in body
                for (form bind) = (multiple-value-list (build-inner expr))
                collect `(enter ,form ,symb)
                do (push bind bindings))
        ,symb)
     (reduce #'append (nreverse bindings)))))

(define-builder border-layout (&rest args)
  (let ((kargs (loop for (k v) on args by #'cddr
                     for test = (find k '(:north :east :south :west :center))
                     unless test collect k
                     unless test collect v))
        (layout (gensym "LAYOUT"))
        (bindings ()))
    (values
     `(let ((,layout (make-instance ',(type-of border-layout) ,@kargs)))
        ,@(loop for place in '(:north :east :south :west :center)
                for expr = (getf args place)
                when expr collect (multiple-value-bind (form bind) (build-inner expr)
                                    (push bind bindings)
                                    `(enter ,form ,layout :place ,place)))
        ,layout)
     (reduce #'append (nreverse bindings)))))
