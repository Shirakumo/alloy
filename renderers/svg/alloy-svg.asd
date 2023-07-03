(asdf:defsystem alloy-svg
  :version "0.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description ""
  :homepage "https://github.com/Shirakumo/alloy"
  :serial T
  :components ((:file "package")
               (:file "renderer")
               (:file "documentation"))
  :depends-on (:documentation-utils
               :alloy-simple
               :colored
               :cl-svg
               :cl-base64)
  :in-order-to ((asdf:test-op (asdf:test-op :alloy-test))))
