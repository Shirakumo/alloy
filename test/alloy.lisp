#|
 This file is a part of Alloy
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage #:org.shirakumo.alloy.test
  (:use #:cl #:parachute)
  (:local-nicknames
   (#:alloy #:org.shirakumo.alloy))
  (:export))

(in-package #:org.shirakumo.alloy.test)

(define-test alloy)
