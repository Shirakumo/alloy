(asdf:defsystem alloy-windowing
  :version "0.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description ""
  :homepage "https://shirakumo.org/project/alloy"
  :serial T
  :components ((:file "package")
               (:file "protocol"))
  :depends-on (:documentation-utils
               :alloy)
  :in-order-to ((asdf:test-op (asdf:test-op :alloy-test))))
