#|
 This file is a part of Alloy
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.alloy)

(defvar *unit-parent*)

(defmacro with-unit-parent (parent &body body)
  `(let ((*unit-parent* ,parent))
     ,@body))

(defstruct (unit
            (:constructor %unit (value))
            (:copier NIL)
            (:predicate NIL))
  (value NIL :type single-float :read-only T))

(defmethod print-object ((unit unit) stream)
  (if *print-readably*
      (prin1 (list (type-of unit) (unit-value unit)) stream)
      (format stream "~a~(~a~)" (unit-value unit) (type-of unit))))

(defmethod make-load-form ((unit unit) &optional env)
  (declare (ignore env))
  `(,(type-of unit) ,(unit-value unit)))

(defun unit (unit-ish)
  (etypecase unit-ish
    (unit unit-ish)
    (real (un unit-ish))))

(define-compiler-macro unit (unit-ish &environment env)
  (let* ((unit (gensym "UNIT"))
         (inner `(let ((,unit ,unit-ish))
                  (etypecase ,unit
                    (unit ,unit)
                    (real (un ,unit))))))
    (if (constantp unit-ish env)
        `(load-time-value ,inner)
        inner)))

(defgeneric %to-px (unit))
(declaim (ftype (function (T) single-float) to-px %to-px))

(defmethod %to-px ((real real))
  (float real 0f0))

(defun to-px (thing)
  (%to-px thing))

(defun to-un (thing)
  (/ (to-px thing)
     (resolution-scale (renderer *unit-parent*))
     (base-scale (renderer *unit-parent*))))

(define-compiler-macro to-px (thing &environment env)
  (if (constantp thing env)
      `(load-time-value (%to-px ,thing))
      `(%to-px ,thing)))

(defmacro define-unit (name (value) &body conversion)
  (let ((make-fun (intern (format NIL "%~a" name)))
        (env (gensym "ENV")))
    `(progn
       (defstruct (,name
                   (:include unit)
                   (:constructor ,make-fun (value))
                   (:copier NIL)
                   (:predicate NIL)))

       (defun ,name (&optional (,name 1f0))
         (,make-fun (float ,name 0f0)))

       (define-compiler-macro ,name (&optional (,name 1f0) &environment ,env)
         (if (constantp ,name ,env)
             `(,',make-fun (load-time-value (float ,,name 0f0)))
             `(,',make-fun (float ,,name 0f0))))
       
       (defmethod %to-px ((,name ,name))
         (declare (optimize speed))
         (let ((,value (unit-value ,name)))
           (if (= 0 ,value)
               0.0f0
               (progn ,@conversion)))))))

(define-unit px (px)
  px)

(define-unit vw (vw)
  (* vw (to-px (w (root (layout-tree *unit-parent*))))))

(define-unit vh (vh)
  (* vh (to-px (h (root (layout-tree *unit-parent*))))))

(define-unit pw (pw)
  (* pw (to-px (w *unit-parent*))))

(define-unit ph (ph)
  (* ph (to-px (h *unit-parent*))))

(define-unit un (un)
  (* un (resolution-scale (renderer *unit-parent*)) (base-scale (renderer *unit-parent*))))

(define-unit cm (cm)
  (* cm (dots-per-cm (renderer *unit-parent*))))

;;; FIXME: It would be nice if we could preserve unit types
;;;        if the argument units are of the same type. This
;;;        would avoid requiring coercion to PX via the parent.
(defmacro define-unit-op0 (name op)
  `(progn (defun ,name (&rest units)
            (px (apply #',op (loop for unit in units
                                   collect (to-px unit)))))

          (define-compiler-macro ,name (&rest units)
            `(px (,',op ,@(loop for unit in units
                                collect `(to-px ,unit)))))))

(defmacro define-unit-op1 (name op)
  `(progn (defun ,name (unit &rest more-units)
            (apply #',op (to-px unit) (loop for unit in more-units
                                            collect (to-px unit))))

          (define-compiler-macro ,name (unit &rest more-units)
            `(,',op (to-px ,unit) ,@(loop for unit in more-units
                                          collect `(to-px ,unit))))))

(defmacro define-unit-comp (name op)
  `(progn (defun ,name (unit &rest more-units)
            (apply #',op (to-px unit) (loop for unit in more-units
                                            collect (to-px unit))))

          (define-compiler-macro ,name (unit &rest more-units)
            `(,',op (to-px ,unit) ,@(loop for unit in more-units
                                          collect `(to-px ,unit))))))

(define-unit-op0 u+ +)
(define-unit-op0 u* *)
(define-unit-op1 u- -)
(define-unit-op1 u/ /)
(define-unit-comp u= =)
(define-unit-comp u/= /=)
(define-unit-comp u< <)
(define-unit-comp u> >)
(define-unit-comp u<= <=)
(define-unit-comp u>= >=)
