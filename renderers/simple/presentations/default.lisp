#|
 This file is a part of Alloy
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.alloy.renderers.simple.presentations)

(defclass default-look-and-feel (renderer)
  ())

(define-realisation (default-look-and-feel alloy:button)
  ((:background filled-box)
   :extent (alloy:margins))
  ((:border outlined-box)
   :extent (alloy:margins :l -2 :u -2 :r -2 :b -2))
  ((:label text)
   :text (alloy:text alloy:button)
   :extent (alloy:margins :l 1 :u 1 :r 1 :b 1)
   :valign :center
   :halign :center))

(define-style (default-look-and-feel alloy:button)
  (:background
   :fill-color (case (alloy:focus-for alloy:button default-look-and-feel)
                 (:strong (simple:color 1 1 1))
                 (T (simple:color 0 0 0))))
  (:border
   :fill-color (case (alloy:focus-for alloy:button default-look-and-feel)
                 (:strong (simple:color 1 1 1))
                 (T (simple:color 0 0 0 0))))
  (:label
   :fill-color (case (alloy:focus-for alloy:button default-look-and-feel)
                 (:strong (simple:color 0 0 0))
                 (T (simple:color 1 1 1)))))
