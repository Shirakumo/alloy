#|
 This file is a part of Alloy
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.alloy)

;;; Early
(declaim (ftype (function (T) unit) x y w h))

(defvar *unit-parent*)

(defmacro with-unit-parent (parent &body body)
  `(let ((*unit-parent* ,parent))
     ,@body))

(deftype unit ()
  '(or single-float %unit))

(defstruct (%unit
            (:constructor %unit (value))
            (:copier NIL)
            (:predicate NIL))
  (value NIL :type single-float :read-only T))

(defmethod print-object ((unit %unit) stream)
  (prin1 (list (type-of unit) (%unit-value unit)) stream))

(defmethod make-load-form ((unit %unit) &optional env)
  (declare (ignore env))
  `(,(type-of unit) ,(%unit-value unit)))

(defun unit (unit-ish)
  (etypecase unit-ish
    (unit unit-ish)
    (real (float unit-ish 0f0))))

(define-compiler-macro unit (unit-ish &environment env)
  (let* ((unit (gensym "UNIT"))
         (inner `(let ((,unit ,unit-ish))
                  (etypecase ,unit
                    (unit ,unit)
                    (real (float ,unit 0f0))))))
    (if (constantp unit-ish env)
        `(load-time-value ,inner)
        inner)))

(defgeneric %px (unit))
(declaim (ftype (function (T) single-float) px %px))

(defmethod %px ((real real))
  (float real 0f0))

(defun px (thing)
  (%px thing))

(define-compiler-macro px (thing &environment env)
  (if (constantp thing env)
      `(load-time-value (%px ,thing))
      `(%px ,thing)))

(defmacro define-unit-comparator (name op)
  `(progn (defun ,name (unit &rest more-units)
            (apply #',op (px unit) (loop for unit in more-units
                                            collect (px unit))))

          (define-compiler-macro ,name (unit &rest more-units)
            `(,',op (px ,unit) ,@(loop for unit in more-units
                                          collect `(px ,unit))))))

(define-unit-comparator unit= =)
(define-unit-comparator unit/= /=)
(define-unit-comparator unit< <)
(define-unit-comparator unit> >)
(define-unit-comparator unit<= <=)
(define-unit-comparator unit>= >=)

(defmacro define-unit (name (value) &body conversion)
  (let ((make-fun (intern (format NIL "%~a" name)))
        (env (gensym "ENV")))
    `(progn
       (defstruct (,name
                   (:include %unit)
                   (:constructor ,make-fun (value))
                   (:copier NIL)
                   (:predicate NIL)))

       (defun ,name (,name)
         (,make-fun (float ,name 0f0)))

       (define-compiler-macro ,name (,name &environment ,env)
         (if (constantp ,name ,env)
             `(,',make-fun (load-time-value (float ,,name 0f0)))
             `(,',make-fun (float ,,name 0f0))))
       
       (defmethod %px ((,name ,name))
         (declare (optimize speed))
         (let ((,value (%unit-value ,name)))
           ,@conversion)))))

(define-unit vw (vw)
  (* vw (w (root (layout-tree *unit-parent*)))))

(define-unit vh (vh)
  (* vh (h (root (layout-tree *unit-parent*)))))

(define-unit pw (pw)
  (* pw (w *unit-parent*)))

(define-unit ph (ph)
  (* ph (h *unit-parent*)))

(define-unit un (un)
  (* un (resolution-scale (renderer)) (base-scale (renderer *unit-parent*))))

(define-unit cm (cm)
  (* cm (dots-per-cm (renderer *unit-parent*))))
