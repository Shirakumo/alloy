(asdf:defsystem alloy-opengl-fond
  :version "0.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description ""
  :homepage "https://github.com/Shirakumo/alloy"
  :serial T
  :components ((:file "fond"))
  :depends-on (:alloy-opengl
               :font-discovery
               :cl-fond)
  :in-order-to ((asdf:test-op (asdf:test-op :alloy-test))))
