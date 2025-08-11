(asdf:defsystem alloy-simple
  :version "0.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description ""
  :homepage "https://shirakumo.org/project/alloy"
  :serial T
  :components ((:file "package")
               (:file "protocol")
               (:file "transforms")
               (:file "defaults")
               (:file "canvas")
               (:file "documentation"))
  :depends-on (:documentation-utils
               :colored
               :alloy
               #++
               (:feature (:and :sbcl :x86-64) (:require :sb-simd)))
  :in-order-to ((asdf:test-op (asdf:test-op :alloy-test))))
