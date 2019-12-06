#|
 This file is a part of Alloy
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.alloy)

(defclass sidebar (structure)
  ())

(defmethod enter ((element layout-element) (structure sidebar) &key)
  (when (next-method-p) (call-next-method))
  (enter element (layout-element structure) :place :center))

(defmethod enter ((element focus-element) (structure sidebar) &key)
  (when (next-method-p) (call-next-method))
  (enter element (focus-element structure)))

(defmethod initialize-instance :after ((structure sidebar) &key layout focus focus-parent layout-parent (side :west))
  (let* ((side (ecase side
                 (:north :south)
                 (:east :west)
                 (:south :north)
                 (:west :east)))
         (frame (make-instance 'frame :layout-parent layout-parent :padding (margins 0)))
         (focus-list (make-instance 'focus-list :focus-parent focus-parent))
         (dragger (make-instance 'resizer :side side :data frame :focus-parent focus-list)))
    (enter dragger frame :place side)
    (finish-structure structure frame focus-list)
    (when layout
      (enter layout structure))
    (when focus
      (enter focus structure))))

;; FIXME: reinitialize-instance
