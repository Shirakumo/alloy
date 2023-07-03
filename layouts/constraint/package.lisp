(defpackage #:org.shirakumo.alloy.layouts.constraint
  (:use #:cl)
  (:local-nicknames
   (#:alloy #:org.shirakumo.alloy)
   (#:cass #:org.shirakumo.classowary))
  (:export
   #:layout
   #:suggest
   #:constrain))
