(asdf:defsystem alloy-animation
  :version "0.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description ""
  :homepage "https://github.com/Shirakumo/alloy"
  :serial T
  :components ((:file "package")
               (:file "easing")
               (:file "lerp")
               (:file "animation")
               (:file "change"))
  :depends-on (:documentation-utils
               :alloy
               :colored)
  :in-order-to ((asdf:test-op (asdf:test-op :alloy-test))))
