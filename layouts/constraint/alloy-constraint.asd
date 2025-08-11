(asdf:defsystem alloy-constraint
  :version "0.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description ""
  :homepage "https://shirakumo.org/project/alloy"
  :serial T
  :components ((:file "package")
               (:file "constraints")
               (:file "layout")
               (:file "documentation"))
  :depends-on (:documentation-utils
               :alloy
               :classowary)
  :in-order-to ((asdf:test-op (asdf:test-op :alloy-test))))
