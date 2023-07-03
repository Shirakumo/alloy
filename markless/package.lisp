(defpackage #:org.shirakumo.alloy.markless
  (:use #:cl)
  (:shadow #:number)
  (:local-nicknames
   (#:markless #:org.shirakumo.markless)
   (#:component #:org.shirakumo.markless.components)
   (#:alloy #:org.shirakumo.alloy))
  (:export))
