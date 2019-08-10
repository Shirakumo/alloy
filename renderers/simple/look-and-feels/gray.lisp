#|
 This file is a part of Alloy
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.alloy.renderers.simple)

(defclass gray (look-and-feel) ())

(defmethod default-presentation ((gray gray) component)
  (make-instance 'presentation
                 :padding (alloy:margins 2 2 2 2)
                 :text-color (color 0 0 0)
                 :text-alignment :middle
                 :text-vertical-alignment :middle
                 :text-direction :right
                 :text-size 12
                 :font-family "sans-serif"
                 :font-style :normal
                 :font-variant :normal
                 :font-weight :normal
                 :font-stretch :normal
                 :image-size (alloy:size 16 16)
                 :image-fill :stretch
                 :image-alignment :left))

(defmethod default-presentation ((gray gray) (component alloy:label))
  (merge-presentation-into
   (make-instance 'presentation
                  :text-alignment :start)
   (call-next-method)))

(defmethod default-presentation ((gray gray) (component alloy:button))
  (merge-presentation-into
   (make-instance 'presentation
                  :background-color (color 0.95 0.95 0.95)
                  :border-color (color 0 0 0)
                  :border-thickness 1.0)
   (call-next-method)))
