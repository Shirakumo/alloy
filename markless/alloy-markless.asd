(asdf:defsystem alloy-markless
  :version "0.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description ""
  :homepage "https://shirakumo.org/project/alloy"
  :serial T
  :components ((:file "package")
               (:file "markless"))
  :depends-on (:alloy
               :cl-markless))
