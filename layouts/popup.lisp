(in-package #:org.shirakumo.alloy)

(defclass popup (layout-element)
  ())

(defmethod enter ((popup popup) (tree layout-tree) &rest args)
  (apply #'enter popup (popups tree) args))

(defmethod update ((popup popup) (tree layout-tree) &rest args)
  (apply #'update popup (popups tree) args))

(defmethod leave ((popup popup) (tree layout-tree))
  (leave popup (popups tree)))

(defmethod enter ((popup popup) (tree focus-tree) &rest args)
  (apply #'enter popup (popups tree) args))

(defmethod update ((popup popup) (tree focus-tree) &rest args)
  (apply #'update popup (popups tree) args))

(defmethod leave ((popup popup) (tree focus-tree))
  (leave popup (popups tree)))

(defmethod enter ((popup popup) (ui ui) &rest args)
  (when (typep popup 'focus-element)
    (apply #'enter popup (focus-tree ui) args))
  (apply #'enter popup (layout-tree ui) args))

(defmethod update ((popup popup) (ui ui) &rest args)
  (when (typep popup 'focus-element)
    (apply #'update popup (focus-tree ui) args))
  (apply #'update popup (layout-tree ui) args))

(defmethod leave ((popup popup) (ui ui))
  (when (typep popup 'focus-element)
    (leave popup (focus-tree ui)))
  (leave popup (layout-tree ui)))

(defclass tooltip (popup renderable)
  ((component :initarg :component :initform (arg! :component) :accessor component)))

(defmethod text ((tooltip tooltip))
  (tooltip (component tooltip)))

(defmethod focus ((tooltip tooltip))
  NIL)

(defmethod notice-size ((tooltip tooltip) (parent layout))
  (let ((size (suggest-size (bounds tooltip) tooltip)))
    (setf (extent-w (bounds tooltip)) (w size))
    (setf (extent-h (bounds tooltip)) (h size))))
