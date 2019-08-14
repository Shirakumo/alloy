#|
 This file is a part of Alloy
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.alloy.renderers.simple)

(defclass gray (look-and-feel) ())

(defmethod default-presentation ((gray gray) component focus)
  (make-instance 'presentation
                 :padding (alloy:margins :l 2 :u 2 :r 2 :b 2)
                 :text-color (color 0 0 0)
                 :text-alignment :middle
                 :text-vertical-alignment :middle
                 :text-direction :right
                 :text-size 12
                 :font-family ""
                 :font-style :normal
                 :font-variant :normal
                 :font-weight :normal
                 :font-stretch :normal
                 :image-size (alloy:size 16 16)
                 :image-fill :stretch
                 :image-alignment :left))

(defmethod default-presentation ((gray gray) (component alloy:label) focus)
  (merge-presentation-into
   (make-instance 'presentation
                  :text-alignment :start)
   (call-next-method)))

(defmethod default-presentation ((gray gray) (component alloy:button) focus)
  (merge-presentation-into
   (make-instance 'presentation
                  :background-color (ecase focus
                                      ((NIL) (color 0.95 0.95 0.95))
                                      (:weak (color 0.8 0.8 0.8))
                                      (:strong (color 0.2 0.2 0.2)))
                  :text-color (case focus
                                (:strong (color 1 1 1))
                                (T (color 0 0 0)))
                  :border-color (ecase focus
                                  ((NIL) (color 0.75 0.75 0.75))
                                  (:weak (color 0.75 0.75 0.75))
                                  (:strong (color 0 0 0)))
                  :border-thickness 1.0)
   (call-next-method)))
