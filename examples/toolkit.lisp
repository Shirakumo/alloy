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
  (multiple-value-bind (documentation body)
      (typecase body
        ((cons string)
         (values (first body) (rest body)))
        (T
         (values nil body)))
    (let ((thunk (gensym "THUNK")))
      `(progn (defun ,name (,@(rest args))
                ,@(when documentation `(,documentation))
                (flet ((,thunk (,(first args))
                         ,@body))
                  (if (boundp '*screen*)
                      (,thunk *screen*)
                      (glfw:with-screen (*screen* 'screen :base-scale 2.0)
                        (,thunk *screen*)))))
              (unless (pushnew ',name *examples*))))))

#+alloy-framebuffers
(defmacro define-example (name args &body body)
  (let ((thunk (gensym "THUNK")))
    `(progn (defun ,name (,@(rest args))
              (flet ((,thunk (,(first args))
                       ,@body))
                (if (boundp '*screen*)
                    (,thunk *screen*)
                    (framebuffers:with-screen (*screen* 'screen)
                      (,thunk *screen*)))))
            (pushnew ',name *examples*))))

(defun list-examples ()
  (copy-list *examples*))

(defun launch (example &rest args)
  (apply (find-symbol (string example) #.*package*) args))
