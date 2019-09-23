#|
 This file is a part of Alloy
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.alloy)

(defstruct (unit
            (:constructor %unit (value))
            (:copier NIL)
            (:predicate NIL))
  (value NIL :type single-float :read-only T))

(defmethod print-object ((unit unit) stream)
  (prin1 (list (type-of unit) (unit-value unit)) stream))

(defmethod make-load-form ((unit unit) &optional env)
  (declare (ignore env))
  `(,(type-of unit) ,(unit-value unit)))

(defmacro define-unit-comparator (name op)
  `(progn (defun ,name (parent unit &rest more-units)
            (apply #',op (to-px parent unit) (loop for unit in more-units
                                                   collect (to-px parent unit))))

          (define-compiler-macro ,name (parent unit &rest more-units)
            (let ((parentg (gensym "PARENT")))
              `(let ((,parentg ,parent))
                 (,',op (to-px ,unit ,parentg)
                        ,@(loop for unit in more-units
                                collect `(to-px ,unit ,parentg))))))))

(define-unit-comparator unit= =)
(define-unit-comparator unit/= /=)
(define-unit-comparator unit< <)
(define-unit-comparator unit> >)
(define-unit-comparator unit<= <=)
(define-unit-comparator unit>= >=)

(defun unit (unit-ish)
  (etypecase unit-ish
    (unit unit-ish)
    (real (px unit-ish))))

(define-compiler-macro unit (unit-ish &environment env)
  (let* ((unit (gensym "UNIT"))
         (inner `(let ((,unit ,unit-ish))
                  (etypecase ,unit
                    (unit ,unit)
                    (real (px ,unit))))))
    (if (constantp unit-ish env)
        `(load-time-value ,inner)
        inner)))

(declaim (ftype (function (unit T) single-float) to-px))
(defgeneric to-px (unit parent-element))

(defmacro define-unit (name (value parent) &body conversion)
  (let ((make-fun (intern (format NIL "%~a" name)))
        (env (gensym "ENV")))
    `(progn
       (defstruct (,name
                   (:include unit)
                   (:constructor ,make-fun (value))
                   (:copier NIL)
                   (:predicate NIL)))

       (defun ,name (,name)
         (,make-fun (float ,name 0f0)))

       (define-compiler-macro ,name (,name &environment ,env)
         (if (constantp ,name ,env)
             `(,',make-fun (load-time-value (float ,,name 0f0)))
             `(,',make-fun (float ,,name 0f0))))
       
       (defmethod to-px ((,name ,name) ,parent)
         (declare (optimize speed))
         (let ((,value (unit-value ,name)))
           ,@conversion)))))

(define-unit px (px parent)
  px)

(define-unit vw (vw parent)
  (* vw (extent-w (bounds (root (layout-tree parent))))))

(define-unit vh (vh parent)
  (* vh (extent-h (bounds (root (layout-tree parent))))))

(define-unit pw (pw parent)
  (* pw (extent-w (bounds parent))))

(define-unit ph (ph parent)
  (* ph (extent-h (bounds parent))))

(define-unit un (un parent)
  (* un (resolution-scale (renderer parent)) (base-scale (renderer parent))))

(define-unit cm (cm parent)
  (* cm (dots-per-cm (renderer parent))))
