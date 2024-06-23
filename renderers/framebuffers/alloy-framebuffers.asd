(asdf:defsystem alloy-framebuffers
  :version "0.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "Alloy windowing backend implementation using native Framebuffers"
  :homepage "https://github.com/Shirakumo/alloy"
  :serial T
  :components ((:file "package")
               (:file "renderer")
               (:file "windowing")
               (:file "documentation"))
  :depends-on (:documentation-utils
               :alloy-simple
               :alloy-simple-presentations
               :alloy-windowing
               :framebuffers
               :colored)
  :in-order-to ((asdf:test-op (asdf:test-op :alloy-test))))
