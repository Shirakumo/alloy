#|
 This file is a part of Alloy
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

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
  (apply #'enter popup (focus-tree ui) args)
  (apply #'enter popup (layout-tree ui) args))

(defmethod update ((popup popup) (ui ui) &rest args)
  (apply #'update popup (focus-tree ui) args)
  (apply #'update popup (layout-tree ui) args))

(defmethod leave ((popup popup) (ui ui))
  (leave popup (focus-tree ui))
  (leave popup (layout-tree ui)))
