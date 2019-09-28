#|
 This file is a part of Alloy
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.alloy.renderers.simple.presentations)

;; TODO: Pallettes

(defclass default-look-and-feel (renderer)
  ())

(defmethod update-shape ((renderer default-look-and-feel) (renderable alloy:button) (shape text))
  (call-next-method)
  (setf (text shape) (alloy:data renderable)))

(defmethod update-shape ((renderer default-look-and-feel) (renderable alloy:label) (shape text))
  (call-next-method)
  (setf (text shape) (alloy:data renderable)))

(defmethod update-shape ((renderer default-look-and-feel) (renderable alloy:input-line) (shape text))
  (call-next-method)
  (setf (text shape) (alloy:value renderable)))

(defmethod update-shape ((renderer default-look-and-feel) (renderable alloy:progress) (shape text))
  (call-next-method)
  (setf (text shape) (format NIL "~,1f%" (/ (alloy:value renderable) (alloy:maximum renderable) 1/100))))

(define-style (default-look-and-feel renderable)
  (:background
   :fill-color (simple:color 0.15 0.15 0.15))
  (:border
   :fill-color (case alloy:focus
                 ((:weak :strong) (simple:color 0.9 0.9 0.9))
                 (T (simple:color 0 0 0 0)))
   :z-index 1)
  (:label
   :fill-color (case alloy:focus
                 ((:weak :strong) (simple:color 0 0 0))
                 (T (simple:color 1 1 1)))))

(define-realisation (default-look-and-feel alloy:label)
  ((:label text)
   :text (alloy:data alloy:renderable)
   :extent (alloy:margins)))

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
   :fill-color (case alloy:focus
                 (:strong (simple:color 0.9 0.9 0.9))
                 (:weak (simple:color 0.7 0.7 0.7))
                 (T (simple:color 0.25 0.2 0.8)))))

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
   :fill-color (case alloy:focus
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
   :fill-color (case alloy:focus
                 (:strong (simple:color 0.9 0.9 0.9))
                 (:weak (simple:color 0.7 0.7 0.7))
                 (T (simple:color 0.15 0.15 0.15))))
  (:cursor
   :fill-color (case alloy:focus
                 (:strong (simple:color 0 0 0))
                 (T (simple:color 0 0 0 0)))))

(define-realisation (default-look-and-feel alloy:slider)
  ((:background filled-box)
   :extent (alloy:margins))
  ((:border outlined-box)
   :extent (alloy:margins -3))
  ((:handle filled-box)
   :extent (alloy:extent -5 0 10 (alloy:ph))))

(define-style (default-look-and-feel alloy:slider)
  (:handle
   :offset (alloy:point (alloy:pw (/ (- alloy:value (alloy:minimum alloy:renderable))
                                     (- (alloy:maximum alloy:renderable) (alloy:minimum alloy:renderable))))
                        0)
   :fill-color (case alloy:focus
                 (:strong (simple:color 1 1 1))
                 (T (simple:color 0.25 0.2 0.8)))))

(define-realisation (default-look-and-feel alloy:progress)
  ((:background filled-box)
   :extent (alloy:margins))
  ((:bar filled-box)
   :extent (alloy:margins 3))
  ((:label text)
   :text (format NIL "~,1f%" (/ (alloy:value alloy:renderable) (alloy:maximum alloy:renderable) 1/100))
   :extent (alloy:margins 1)
   :halign :middle))

(define-style (default-look-and-feel alloy:progress)
  (:bar
   :fill-color (simple:color 0.25 0.2 0.8)
   :scale (let ((p (/ alloy:value (alloy:maximum alloy:renderable))))
            (alloy:px-size p 1)))
  (:label
   :fill-color (simple:color 1 1 1)))

(define-realisation (default-look-and-feel alloy:radio)
  ((:background filled-circle)
   :extent (alloy:extent 0 0 (alloy:ph 1) (alloy:ph 1)))
  ((:inner filled-circle)
   :extent (alloy:extent (alloy:ph 0.1) (alloy:ph 0.1) (alloy:ph 0.8) (alloy:ph 0.8)))
  ((:border outlined-circle)
   :extent (alloy:extent (alloy:ph -0.1) (alloy:ph -0.1) (alloy:ph 1.2) (alloy:ph 1.2))))

(define-style (default-look-and-feel alloy:radio)
  (:inner
   :fill-color (if (alloy:active-p alloy:renderable)
                   (simple:color 0.25 0.2 0.8)
                   (simple:color 0 0 0 0))))
