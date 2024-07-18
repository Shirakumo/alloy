(in-package #:org.shirakumo.alloy.examples)

(defvar *screen*)
(defvar *examples* ())
(defvar *here* #.(make-pathname :name NIL :type NIL :defaults (or *compile-file-truename* *load-truename*)))

(defun file (path)
  (merge-pathnames path *here*))

(defclass screen (#+alloy-glfw glfw:screen
                  #+alloy-framebuffers framebuffers:screen)
  ())

#+alloy-glfw
(defmacro define-example (name args &body body)
  (let ((thunk (gensym "THUNK")))
    `(progn (defun ,name (,@(rest args))
              (flet ((,thunk (,(first args))
                       ,@body))
                (if (boundp '*screen*)
                    (,thunk *screen*)
                    (glfw:with-screen (*screen* 'screen :base-scale 2.0)
                      (,thunk *screen*)))))
            (pushnew ',name *examples*))))

#+alloy-framebuffers
(defmacro define-example (name args &body body)
  (let ((thunk (gensym "THUNK")))
    `(progn (defun ,name (,@(rest args))
              (flet ((,thunk (,(first args))
                       ,@body))
                (if (boundp '*screen*)
                    (,thunk *screen*)
                    (framebuffers:with-screen (*screen* 'screen)
                      (thunk *screen*)))))
            (pushnew ',name *examples*))))
