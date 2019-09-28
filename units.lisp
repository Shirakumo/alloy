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
  ;; KLUDGE: To avoid infinite recursion, we guess the struct constructor.
  (let ((constructor (intern (format NIL "%~a" (string (type-of unit)))
                             (symbol-package (type-of unit)))))
    `(,constructor ,(unit-value unit))))

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
  (destructuring-bind (to-px from-px) conversion
    (let* ((make-fun (intern (format NIL "%~a" name)))
           (whole (gensym "WHOLE"))
           (env (gensym "ENV"))
           (constructor `(etypecase ,name
                           (real (,make-fun (float ,name 0f0)))
                           (,name ,name)
                           (unit (,make-fun
                                  (let ((px (%to-px ,name)))
                                    ,from-px))))))
      `(progn
         (defstruct (,name
                     (:include unit)
                     (:constructor ,make-fun (value))
                     (:copier NIL)
                     (:predicate NIL)))

         (defun ,name (&optional (,name 1f0))
           ,constructor)

         (define-compiler-macro ,name (&whole ,whole &optional (,name 1f0) &environment ,env)
           (if (constantp ,name ,env)
               (list 'load-time-value ,constructor)
               ,whole))
         
         (defmethod %to-px ((,name ,name))
           (let ((,value (unit-value ,name)))
             (if (= 0 ,value)
                 0.0f0
                 ,to-px)))))))

(define-unit px (px)
  px
  px)

(define-unit vw (vw)
  (* vw (to-px (w (root (layout-tree *unit-parent*)))))
  (/ px (to-px (w (root (layout-tree *unit-parent*))))))

(define-unit vh (vh)
  (* vh (to-px (h (root (layout-tree *unit-parent*)))))
  (/ px (to-px (h (root (layout-tree *unit-parent*))))))

(define-unit pw (pw)
  (* pw (to-px (w *unit-parent*)))
  (/ px (to-px (w *unit-parent*))))

(define-unit ph (ph)
  (* ph (to-px (h *unit-parent*)))
  (/ px (to-px (h *unit-parent*))))

(define-unit un (un)
  (* un (resolution-scale (renderer *unit-parent*)) (base-scale (renderer *unit-parent*)))
  (/ px (resolution-scale (renderer *unit-parent*)) (base-scale (renderer *unit-parent*))))

(define-unit cm (cm)
  (* cm (dots-per-cm (renderer *unit-parent*)))
  (/ px (dots-per-cm (renderer *unit-parent*))))

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
(define-unit-op1 umax max)
(define-unit-op1 umin min)
(define-unit-comp u= =)
(define-unit-comp u/= /=)
(define-unit-comp u< <)
(define-unit-comp u> >)
(define-unit-comp u<= <=)
(define-unit-comp u>= >=)
