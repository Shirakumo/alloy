(asdf:defsystem alloy-test
  :version "0.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "Tests for the Alloy UI protocol."
  :homepage "https://shirakumo.org/project/alloy"
  :serial T
  :components ((:file "alloy")
               (:file "container")
               (:file "focus-tree")
               (:file "geometry"))
  :depends-on (:alloy
               :alexandria
               :parachute)
  :perform (asdf:test-op (op c) (uiop:symbol-call :parachute :test :org.shirakumo.alloy.test)))
