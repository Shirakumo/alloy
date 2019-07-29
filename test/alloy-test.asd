#|
 This file is a part of Alloy
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(asdf:defsystem alloy-test
  :version "0.0.0"
  :license "zlib"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "Tests for the Alloy UI protocol."
  :homepage "https://github.com/Shirakumo/alloy"
  :serial T
  :components ((:file "alloy")
               (:file "container")
               (:file "focus-tree")
               (:file "geometry"))
  :depends-on (:alloy
               :parachute)
  :perform (asdf:test-op (op c) (uiop:symbol-call :parachute :test :org.shirakumo.alloy.test)))
