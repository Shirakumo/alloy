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
             (,make-fun (load-time-value (float ,name 0f0)))
             (,make-fun (float ,name 0f0))))
       
       (defmethod to-px ((,name ,name) ,parent)
         (declare (optimize speed))
         (let ((,value (unit-value ,name)))
           ,@conversion)))))

(define-unit px (px parent)
  px)

(define-unit vw (vw parent)
  (* vw (base-scale (renderer parent)) (extent-w (bounds (root (layout-tree parent))))))

(define-unit vh (vh parent)
  (* vh (base-scale (renderer parent)) (extent-h (bounds (root (layout-tree parent))))))

(define-unit pw (pw parent)
  (* pw (base-scale (renderer parent)) (extent-w (bounds parent))))

(define-unit ph (ph parent)
  (* ph (base-scale (renderer parent)) (extent-h (bounds parent))))

(define-unit un (un parent)
  (* un (base-scale (renderer parent)) (resolution-scale (renderer parent))))

(define-unit cm (cm parent)
  (* cm (pixels-per-cm (layout-tree parent))))
