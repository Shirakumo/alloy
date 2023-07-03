(asdf:defsystem alloy-opengl-png
  :version "0.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description ""
  :homepage "https://github.com/Shirakumo/alloy"
  :serial T
  :components ((:file "png"))
  :depends-on (:alloy-opengl
               :pngload)
  :in-order-to ((asdf:test-op (asdf:test-op :alloy-test))))
