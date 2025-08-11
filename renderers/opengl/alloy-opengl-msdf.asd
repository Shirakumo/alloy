(asdf:defsystem alloy-opengl-msdf
  :version "0.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description ""
  :homepage "https://shirakumo.org/project/alloy"
  :serial T
  :components ((:file "msdf"))
  :depends-on (:alloy-opengl
               :alloy-simple-presentations
               :alloy-animation
               :font-discovery
               :uax-14
               :sdf/bmfont
               :3b-bmfont
               :3b-bmfont/json)
  :in-order-to ((asdf:test-op (asdf:test-op :alloy-test))))
