#|
 This file is a part of Alloy
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.alloy.renderers.simple.presentations)

(defclass default-look-and-feel (renderer)
  ())

(define-style (default-look-and-feel T)
  (:background
   :fill-color (case (alloy:focus)
                 (:strong (simple:color 1 1 1))
                 (T (simple:color 0.24 0.19 0.79))))
  (:border
   :fill-color (case (alloy:focus)
                 (:strong (simple:color 1 1 1))
                 (T (simple:color 0 0 0 0))))
  (:label
   :fill-color (case (alloy:focus)
                 (:strong (simple:color 0 0 0))
                 (T (simple:color 1 1 1)))))

(define-realisation (default-look-and-feel alloy:label)
  ((:label text)
   :text (alloy:text alloy:component)
   :extent (alloy:margins)))

(define-realisation (default-look-and-feel alloy:button)
  ((:background filled-box)
   :extent (alloy:margins))
  ((:border outlined-box)
   :extent (alloy:margins :l -2 :u -2 :r -2 :b -2))
  ((:label text)
   :text (alloy:text alloy:component)
   :extent (alloy:margins :l 1 :u 1 :r 1 :b 1)
   :halign :center))

(define-realisation (default-look-and-feel alloy:switch)
  ((:background filled-box)
   :extent (alloy:margins))
  ((:border outlined-box)
   :extent (alloy:margins))
  ((:switch filled-box)
   ;; FIXME: This is not good.
   :extent (alloy:extent 0 0 20 20)))

(define-style (default-look-and-feel alloy:switch)
  (:switch
   :offset (alloy:point (if (alloy:state alloy:component)
                            (- (alloy:extent-w (alloy:bounds)) 20)
                            0))
   :fill-color (case (alloy:focus)
                 (:strong (simple:color 1 1 1))
                 (T (simple:color 0 0 0 0)))))

(define-realisation (default-look-and-feel alloy:input-line)
  ((:background filled-box)
   :extent (alloy:margins))
  ((:border outlined-box)
   :extent (alloy:margins))
  ((:label text)
   :text (alloy:text alloy:component)
   :extent (alloy:margins :l 1 :u 1 :r 1 :b 1)))

(define-style (default-look-and-feel alloy:input-line)
  (:background
   :fill-color (case (alloy:focus)
                 (:strong (simple:color 1 1 1))
                 (T (simple:color 0.3 0.3 0.3))))
  (:label
   :fill-color (simple:color 0 0 0)))
