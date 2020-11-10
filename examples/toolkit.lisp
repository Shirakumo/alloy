#|
 This file is a part of Alloy
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.alloy.examples)

(defvar *screen*)

(defclass screen (glfw:screen) ())

(defmacro define-example (name args &body body)
  (let ((thunk (gensym "THUNK")))
    `(defun ,name (,@(rest args))
       (flet ((,thunk (,(first args))
                ,@body))
         (if (boundp '*screen*)
             (,thunk *screen*)
             (glfw:with-screen (*screen* 'screen :base-scale 2.0)
               (,thunk *screen*)))))))
