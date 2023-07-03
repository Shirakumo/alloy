(asdf:defsystem alloy-opengl
  :version "0.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description ""
  :homepage "https://github.com/Shirakumo/alloy"
  :serial T
  :components ((:file "package")
               (:file "protocol")
               (:file "renderer")
               (:file "gradient")
               (:file "unmanaged")
               (:file "documentation"))
  :depends-on (:documentation-utils
               :alloy-simple
               :cl-opengl
               :alexandria)
  :in-order-to ((asdf:test-op (asdf:test-op :alloy-test))))
