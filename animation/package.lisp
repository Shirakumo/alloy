#|
 This file is a part of Alloy
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage #:org.shirakumo.alloy.animation
  (:use #:cl)
  (:local-nicknames
   (#:alloy #:org.shirakumo.alloy)
   (#:colored #:org.shirakumo.alloy.colored))
  ;; animation.lisp
  (:export
   #:animation
   #:list-animation
   #:update
   #:apply-animation
   #:animated
   #:define-animation)
  ;; easing.lisp
  (:export
   #:easing
   #:ease
   #:linear
   #:quad-in
   #:quad-out
   #:quad-in-out
   #:cubic-in
   #:cubic-out
   #:cubic-in-out
   #:quart-in
   #:quart-out
   #:quart-in-out
   #:quint-in
   #:quint-out
   #:quint-in-out
   #:sine-in
   #:sine-out
   #:sine-in-out
   #:expo-in
   #:expo-out
   #:expo-in-out
   #:circ-in
   #:circ-out
   #:circ-in-out
   #:back-in
   #:back-out
   #:back-in-out
   #:elastic-in
   #:elastic-out
   #:elastic-in-out
   #:bounce-in
   #:bounce-out
   #:bounce-in-out)
  ;; lerp.lisp
  (:export
   #:lerp)
  ;; change.lisp
  (:export
   #:compile-change-tracker))
