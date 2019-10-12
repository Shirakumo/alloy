#|
 This file is a part of Alloy
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(asdf:defsystem alloy-glfw
  :version "0.0.0"
  :license "zlib"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
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
               :alloy-opengl
               :alloy-windowing
               :cl-opengl
               :cl-glfw3)
  :in-order-to ((asdf:test-op (asdf:test-op :alloy-test))))
