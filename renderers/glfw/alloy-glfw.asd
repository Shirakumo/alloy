(asdf:defsystem alloy-glfw
  :version "0.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description ""
  :homepage "https://github.com/Shirakumo/alloy"
  :serial T
  :components ((:file "package")
               (:file "fixup")
               (:file "renderer")
               (:file "windowing")
               (:file "documentation"))
  :depends-on (:documentation-utils
               :alloy-simple
               :alloy-simple-presentations
               :alloy-opengl
               :alloy-opengl-msdf
               :alloy-opengl-png
               :alloy-windowing
               :float-features
               :cl-opengl
               :cl-glfw3
               :colored)
  :in-order-to ((asdf:test-op (asdf:test-op :alloy-test))))
