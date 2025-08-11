(asdf:defsystem alloy-opengl
  :version "0.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description ""
  :homepage "https://shirakumo.org/project/alloy"
  :serial T
  :components ((:file "package")
               (:file "protocol")
               (:file "renderer")
               (:file "gradient")
               (:file "unmanaged")
               (:file "documentation"))
  :depends-on (:documentation-utils
               :memory-regions/region
               :alloy-simple
               :cl-opengl
               :alexandria)
  :in-order-to ((asdf:test-op (asdf:test-op :alloy-test))))
