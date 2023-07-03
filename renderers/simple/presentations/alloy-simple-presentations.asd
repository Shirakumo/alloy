(asdf:defsystem alloy-simple-presentations
  :version "0.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description ""
  :homepage "https://github.com/Shirakumo/alloy"
  :serial T
  :components ((:file "package")
               (:file "protocol")
               (:file "default")
               (:file "documentation"))
  :depends-on (:documentation-utils
               :stealth-mixin
               :colored
               :alloy-simple
               :alloy-animation)
  :in-order-to ((asdf:test-op (asdf:test-op :alloy-test))))
