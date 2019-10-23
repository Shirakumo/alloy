#|
 This file is a part of Alloy
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.alloy.renderers.simple.presentations)

;; TODO: Pallettes

(defclass default-look-and-feel (renderer)
  ())

(define-style (default-look-and-feel renderable)
  (:background
   :pattern (colored:color 0.15 0.15 0.15))
  (:border
   :pattern (colored:color 0.9 0.9 0.9)
   :hidden-p (null alloy:focus)
   :z-index 1)
  (:label
   :pattern (case alloy:focus
              ((:weak :strong) colors:black)
              (T colors:white))))

(define-realization (default-look-and-feel alloy:label)
  ((:label text)
   :text (alloy:data alloy:renderable)
   :extent (alloy:margins)))

(define-style (default-look-and-feel alloy:label)
  (:label
   :text (alloy:data alloy:renderable)))

(define-realization (default-look-and-feel alloy:button)
  ((:background filled-box)
   :extent (alloy:margins))
  ((:border outlined-box)
   :extent (alloy:margins -3))
  ((:label text)
   :text (alloy:data alloy:renderable)
   :extent (alloy:margins 1)
   :halign :middle))

(define-style (default-look-and-feel alloy:button)
  (:background
   :pattern (case alloy:focus
                 (:strong (colored:color 0.9 0.9 0.9))
                 (:weak (colored:color 0.7 0.7 0.7))
                 (T (colored:color 0.25 0.2 0.8))))
  (:label
   :text (alloy:data alloy:renderable)))

(define-realization (default-look-and-feel alloy:switch)
  ((:background filled-box)
   :extent (alloy:margins))
  ((:border outlined-box)
   :extent (alloy:margins -3))
  ((:switch filled-box)
   :extent (alloy:extent 0 0 (alloy:pw 0.3) (alloy:ph))))

(define-style (default-look-and-feel alloy:switch)
  (:switch
   :offset (alloy:point (if alloy:value
                            (alloy:pw 0.7)
                            0))
   :pattern (case alloy:focus
                 (:strong colors:white)
                 (T (colored:color 0.25 0.2 0.8)))))

(define-realization (default-look-and-feel alloy:input-line)
  ((:background filled-box)
   :extent (alloy:margins))
  ((:border outlined-box)
   :extent (alloy:margins -3))
  ((:label text)
   :text alloy:value
   :extent (alloy:margins 1))
  ((:cursor filled-box)
   :extent (alloy:extent 0 (alloy:ph 0.15) 1 (alloy:ph 0.7))))

(define-style (default-look-and-feel alloy:input-line)
  (:background
   :pattern (case alloy:focus
                 (:strong (colored:color 0.9 0.9 0.9))
                 (:weak (colored:color 0.7 0.7 0.7))
                 (T (colored:color 0.15 0.15 0.15))))
  (:cursor
   :hidden-p (null alloy:focus)
   :pattern colors:black)
  (:label
   :text (alloy:data alloy:renderable)))

(define-realization (default-look-and-feel alloy:slider)
  ((:background filled-box)
   :extent (ecase (alloy:orientation alloy:renderable)
             (:horizontal (alloy:extent 0 (alloy:ph 0.4) (alloy:pw) (alloy:ph 0.2)))
             (:vertical (alloy:extent (alloy:pw 0.4) 0 (alloy:pw 0.2) (alloy:ph)))))
  ((:border outlined-box)
   :extent (alloy:margins -3))
  ((:handle filled-box)
   :extent (ecase (alloy:orientation alloy:renderable)
             (:horizontal (alloy:extent -5 0 10 (alloy:ph)))
             (:vertical (alloy:extent 0 -5 (alloy:pw) 10)))))

(define-style (default-look-and-feel alloy:slider)
  (:handle
   :offset (ecase (alloy:orientation alloy:renderable)
             (:horizontal (alloy:point (alloy:pw (alloy:slider-unit alloy:renderable)) 0))
             (:vertical (alloy:point 0 (alloy:ph (alloy:slider-unit alloy:renderable)))))
   :pattern (case alloy:focus
                 (:strong colors:white)
                 (T (colored:color 0.25 0.2 0.8)))))

(define-realization (default-look-and-feel alloy:progress)
  ((:background filled-box)
   :extent (alloy:margins))
  ((:bar filled-box)
   :extent (alloy:margins 3))
  ((:label text)
   :extent (alloy:margins 1)
   :halign :middle))

(define-style (default-look-and-feel alloy:progress)
  (:bar
   :pattern (colored:color 0.25 0.2 0.8)
   :scale (let ((p (/ alloy:value (alloy:maximum alloy:renderable))))
            (alloy:px-size p 1)))
  (:label
   :text (format NIL "~,1f%" (/ (alloy:value alloy:renderable) (alloy:maximum alloy:renderable) 1/100))
   :pattern colors:white))

(define-realization (default-look-and-feel alloy:radio)
  ((:background filled-circle)
   :extent (alloy:extent 0 0 (alloy:ph 1) (alloy:ph 1)))
  ((:inner filled-circle)
   :extent (alloy:extent (alloy:ph 0.1) (alloy:ph 0.1) (alloy:ph 0.8) (alloy:ph 0.8)))
  ((:border outlined-circle)
   :extent (alloy:extent (alloy:ph -0.1) (alloy:ph -0.1) (alloy:ph 1.2) (alloy:ph 1.2))))

(define-style (default-look-and-feel alloy:radio)
  (:inner
   :hidden-p (not (alloy:active-p alloy:renderable))
   :pattern (colored:color 0.25 0.2 0.8)))

(define-realization (default-look-and-feel alloy:combo)
  ((:background filled-box)
   :extent (alloy:margins))
  ((:border outlined-box)
   :extent (alloy:margins -3))
  ((:label text)
   :text (princ-to-string (alloy:value alloy:renderable))
   :extent (alloy:margins 1)))

(define-style (default-look-and-feel alloy:combo)
  (:label
   :pattern colors:white
   :text (princ-to-string (alloy:value alloy:renderable))))

(define-realization (default-look-and-feel alloy:combo-item)
  ((:background filled-box)
   :extent (alloy:margins))
  ((:label text)
   :text (alloy:data alloy:renderable)
   :extent (alloy:margins 1)))

(define-style (default-look-and-feel alloy:combo-item)
  (:background
   :pattern (case (alloy:focus alloy:renderable)
                 ((:weak :strong) (colored:color 0.25 0.2 0.8))
                 ((NIL) (colored:color 0.15 0.15 0.15))))
  (:label
   :pattern colors:white))

(define-realization (default-look-and-feel alloy:scrollbar)
  ((:background filled-box)
   :extent (alloy:margins))
  ((:handle filled-box)
   :extent (ecase (alloy:orientation alloy:renderable)
             (:horizontal (alloy:extent -10 0 20 (alloy:ph)))
             (:vertical (alloy:extent 0 -10 (alloy:pw) 20)))))
