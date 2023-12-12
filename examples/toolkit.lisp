(in-package #:org.shirakumo.alloy.examples)

(defvar *screen*)
(defvar *examples* ())
(defvar *here* #.(make-pathname :name NIL :type NIL :defaults (or *compile-file-truename* *load-truename*)))

(defun file (path)
  (merge-pathnames path *here*))

(defclass screen (glfw:screen) ())

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


(defun butwithlast (list)
  (let* ((last)
	 (beginning
	   (loop for item in list
		 for index from 0
		 when (eq index (- (length list) 1)) do (setf last item) until last
		 collect item)))
    (values beginning last)))

(defmacro mk-hash-table (&rest args)
  (let ((table (gensym)))
    `(let ((,table (make-hash-table :test 'eql)))
       ,@(loop for (key value) on args by #'cddr
	       collect `(setf (gethash ,key ,table) ,value))
       ,table)))
