#|
 This file is a part of Alloy
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(asdf:defsystem alloy-opengl-msdf
  :version "0.0.0"
  :license "zlib"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description ""
  :homepage "https://github.com/Shirakumo/alloy"
  :serial T
  :components ((:file "msdf"))
  :depends-on (:alloy-opengl
               :font-discovery
               :sdf/bmfont
               :3b-bmfont
               :3b-bmfont/json)
  :in-order-to ((asdf:test-op (asdf:test-op :alloy-test))))
