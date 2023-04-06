#|
 This file is a part of Alloy
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage #:org.shirakumo.alloy.markless
  (:use #:cl)
  (:shadow #:number)
  (:local-nicknames
   (#:markless #:org.shirakumo.markless)
   (#:component #:org.shirakumo.markless.components)
   (#:alloy #:org.shirakumo.alloy))
  (:export))
