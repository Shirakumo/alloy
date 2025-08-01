(asdf:defsystem alloy-examples
  :version "0.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "Example programs using Alloy"
  :homepage "https://github.com/Shirakumo/alloy"
  :serial T
  :components ((:file "package")
               (:file "toolkit")
               (:file "windows")
               (:file "drop")
               (:file "constraint")
               (:file "animation")
               (:file "menu")
               (:file "font-mixing")
               (:file "fonts")
               (:file "canvas")
               (:file "gradient")
               (:file "grid-bag-layout")
               (:file "sizing")
               (:file "virtual-keyboard")
               (:file "components")
               (:file "sections")
               (:file "alloy-examples"))
  :depends-on (:alloy-glfw
               :alloy-constraint
               :font-discovery))
