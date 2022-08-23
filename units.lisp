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
  (value NIL :type single-float))

(defmethod print-object ((unit unit) stream)
  (if *print-readably*
      (prin1 (list (type-of unit) (unit-value unit)) stream)
      (format stream "~a~(~a~)" (unit-value unit) (type-of unit))))

(defmethod make-load-form ((unit unit) &optional env)
  (declare (ignore env))
  ;; KLUDGE: To avoid infinite recursion, we guess the struct constructor.
  (let ((constructor (intern (format NIL "%~a" (string (type-of unit)))
                             (symbol-package (type-of unit)))))
    `(,constructor ,(unit-value unit))))

(defun unit (unit-ish)
  (etypecase unit-ish
    (unit unit-ish)
    (real (%un (float unit-ish 0f0)))))

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
(cl:declaim (ftype (function (T) single-float) to-px %to-px))

(defmethod %to-px ((real real))
  (float real 0f0))

(defun to-px (thing)
  (%to-px thing))

(define-compiler-macro to-px (thing &environment env)
  (if (constantp thing env)
      `(load-time-value (%to-px ,thing))
      `(%to-px ,thing)))

(defun to-un (thing)
  (/ (%to-px thing)
     (resolution-scale (ui (layout-tree *unit-parent*)))
     (base-scale (ui (layout-tree *unit-parent*)))))

(defmacro define-unit (name (value) &body conversion)
  (destructuring-bind (to-px from-px) conversion
    (let* ((make-fun (intern (format NIL "%~a" name)))
           (whole (gensym "WHOLE"))
           (env (gensym "ENV")))
      `(progn
         (cl:declaim (inline ,make-fun))
         (defstruct (,name
                     (:include unit)
                     (:constructor ,make-fun (value))
                     (:copier NIL)
                     (:predicate NIL)))

         (defun ,name (&optional (,name 1f0))
           (etypecase ,name
             (real (,make-fun (float ,name 0f0)))
             (,name ,name)
             (unit (,make-fun
                    (let ((px (%to-px ,name)))
                      ,from-px)))))

         (define-compiler-macro ,name (&whole ,whole &optional (,name 1f0) &environment ,env)
           (if (constantp ,name ,env)
               `(load-time-value
                 (etypecase ,,name
                   (real (,',make-fun (float ,,name 0f0)))
                   (,',name ,,name)
                   (unit (,',make-fun
                          (let ((px (%to-px ,,name)))
                            ,',from-px)))))
               ,whole))
         
         (defmethod %to-px ((,name ,name))
           (let ((,value (unit-value ,name)))
             (if (= 0 ,value)
                 0.0f0
                 ,to-px)))))))

;;; Early
(cl:declaim (ftype (function (T) single-float) pxx pxy pxw pxh pxl pxu pxr pxb))

(define-unit px (px)
  (float px 0f0)
  px)

(define-unit vw (vw)
  (* vw (pxw (root (layout-tree *unit-parent*))))
  (/ px (pxw (root (layout-tree *unit-parent*)))))

(define-unit vh (vh)
  (* vh (pxh (root (layout-tree *unit-parent*))))
  (/ px (pxh (root (layout-tree *unit-parent*)))))

(define-unit pw (pw)
  (* pw (pxw *unit-parent*))
  (/ px (pxw *unit-parent*)))

(define-unit ph (ph)
  (* ph (pxh *unit-parent*))
  (/ px (pxh *unit-parent*)))

(define-unit un (un)
  (* un (resolution-scale (ui (layout-tree *unit-parent*))) (base-scale (ui (layout-tree *unit-parent*))))
  (/ px (resolution-scale (ui (layout-tree *unit-parent*))) (base-scale (ui (layout-tree *unit-parent*)))))

(define-unit cm (cm)
  (* cm (dots-per-cm (ui (layout-tree *unit-parent*))))
  (/ px (dots-per-cm (ui (layout-tree *unit-parent*)))))

;;; TODO: It would be nice if we could preserve unit types
;;;       if the argument units are of the same type. This
;;;       would avoid requiring coercion to PX via the parent.
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
(define-unit-op1 umax max)
(define-unit-op1 umin min)
(define-unit-comp u= =)
(define-unit-comp u/= /=)
(define-unit-comp u< <)
(define-unit-comp u> >)
(define-unit-comp u<= <=)
(define-unit-comp u>= >=)
