(in-package #:org.shirakumo.alloy)

(defclass sidebar (structure)
  ((dragger :reader dragger)))

(defmethod enter ((element layout-element) (structure sidebar) &rest args &key (place :center))
  (when (next-method-p) (call-next-method))
  (apply #'enter element (layout-element structure) :place place args))

(defmethod enter ((element focus-element) (structure sidebar) &key)
  (when (next-method-p) (call-next-method))
  (enter element (focus-element structure)))

(defmethod initialize-instance :after ((structure sidebar) &key layout focus focus-parent layout-parent (side :west))
  (let* ((opposite (ecase side
                     (:north :south)
                     (:east :west)
                     (:south :north)
                     (:west :east)))
         (frame (make-instance 'frame :padding (margins 0)))
         (focus-list (make-instance 'focus-list))
         (dragger (make-instance 'resizer :side opposite :data frame)))
    (setf (slot-value structure 'dragger) dragger)
    (enter dragger frame :place opposite :size (un 20))
    (finish-structure structure frame focus-list)
    (when layout
      (enter layout structure))
    (when focus
      (enter focus structure))
    (when layout-parent
      (enter frame layout-parent :place side))
    (when focus-parent
      (enter focus-list focus-parent :place side)
      (enter dragger focus-parent :place side))))

(defmethod enter :after ((structure sidebar) (focus focus-element) &key)
  (enter (dragger structure) focus))

;; FIXME: reinitialize-instance
