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
   :pattern (simple:color 0.15 0.15 0.15))
  (:border
   :pattern (case alloy:focus
              ((:weak :strong) (simple:color 0.9 0.9 0.9))
              (T (simple:color 0 0 0 0)))
   :z-index 1)
  (:label
   :pattern (case alloy:focus
              ((:weak :strong) (simple:color 0 0 0))
              (T (simple:color 1 1 1)))))

(define-realisation (default-look-and-feel alloy:label)
  ((:label text)
   :text (alloy:data alloy:renderable)
   :extent (alloy:margins)))

(define-style (default-look-and-feel alloy:label)
  (:label
   :text (alloy:data alloy:renderable)))

(define-realisation (default-look-and-feel alloy:button)
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
                 (:strong (simple:color 0.9 0.9 0.9))
                 (:weak (simple:color 0.7 0.7 0.7))
                 (T (simple:color 0.25 0.2 0.8))))
  (:label
   :text (alloy:data alloy:renderable)))

(define-realisation (default-look-and-feel alloy:switch)
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
                 (:strong (simple:color 1 1 1))
                 (T (simple:color 0.25 0.2 0.8)))))

(define-realisation (default-look-and-feel alloy:input-line)
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
                 (:strong (simple:color 0.9 0.9 0.9))
                 (:weak (simple:color 0.7 0.7 0.7))
                 (T (simple:color 0.15 0.15 0.15))))
  (:cursor
   :pattern (case alloy:focus
                 (:strong (simple:color 0 0 0))
                 (T (simple:color 0 0 0 0))))
  (:label
   :text (alloy:data renderable)))

(define-realisation (default-look-and-feel alloy:slider)
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
                 (:strong (simple:color 1 1 1))
                 (T (simple:color 0.25 0.2 0.8)))))

(define-realisation (default-look-and-feel alloy:progress)
  ((:background filled-box)
   :extent (alloy:margins))
  ((:bar filled-box)
   :extent (alloy:margins 3))
  ((:label text)
   :extent (alloy:margins 1)
   :halign :middle))

(define-style (default-look-and-feel alloy:progress)
  (:bar
   :pattern (simple:color 0.25 0.2 0.8)
   :scale (let ((p (/ alloy:value (alloy:maximum alloy:renderable))))
            (alloy:px-size p 1)))
  (:label
   :text (format NIL "~,1f%" (/ (alloy:value renderable) (alloy:maximum renderable) 1/100))
   :pattern (simple:color 1 1 1)))

(define-realisation (default-look-and-feel alloy:radio)
  ((:background filled-circle)
   :extent (alloy:extent 0 0 (alloy:ph 1) (alloy:ph 1)))
  ((:inner filled-circle)
   :extent (alloy:extent (alloy:ph 0.1) (alloy:ph 0.1) (alloy:ph 0.8) (alloy:ph 0.8)))
  ((:border outlined-circle)
   :extent (alloy:extent (alloy:ph -0.1) (alloy:ph -0.1) (alloy:ph 1.2) (alloy:ph 1.2))))

(define-style (default-look-and-feel alloy:radio)
  (:inner
   :pattern (if (alloy:active-p alloy:renderable)
                   (simple:color 0.25 0.2 0.8)
                   (simple:color 0 0 0 0))))

(define-realisation (default-look-and-feel alloy:combo)
  ((:background filled-box)
   :extent (alloy:margins))
  ((:border outlined-box)
   :extent (alloy:margins -3))
  ((:label text)
   :text (princ-to-string (alloy:value alloy:renderable))
   :extent (alloy:margins 1)))

(define-style (default-look-and-feel alloy:combo)
  (:label
   :pattern (simple:color 1 1 1)
   :text (princ-to-string (alloy:value alloy:renderable))))

(define-realisation (default-look-and-feel alloy:combo-item)
  ((:background filled-box)
   :extent (alloy:margins))
  ((:label text)
   :text (alloy:data alloy:renderable)
   :extent (alloy:margins 1)))

(define-style (default-look-and-feel alloy:combo-item)
  (:background
   :pattern (case (alloy:focus alloy:renderable)
                 ((:weak :strong) (simple:color 0.25 0.2 0.8))
                 ((NIL) (simple:color 0.15 0.15 0.15))))
  (:label
   :pattern (simple:color 1 1 1)))

(define-realisation (default-look-and-feel alloy:scrollbar)
  ((:background filled-box)
   :extent (alloy:margins))
  ((:handle filled-box)
   :extent (ecase (alloy:orientation alloy:renderable)
             (:horizontal (alloy:extent -10 0 20 (alloy:ph)))
             (:vertical (alloy:extent 0 -10 (alloy:pw) 20)))))
