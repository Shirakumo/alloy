#|
This file is a part of Alloy
(c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(asdf:defsystem alloy
  :version "0.0.0"
  :license "zlib"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description ""
  :homepage "https://github.com/Shirakumo/alloy"
  :serial T
  :components ((:file "package")
               (:file "conditions")
               (:file "units")
               (:file "geometry")
               (:file "events")
               (:file "observable")
               (:file "renderer")
               (:file "data")
               (:file "component")
               (:file "container")
               (:file "focus-tree")
               (:file "layout")
               (:file "ui")
               (:module "layouts"
                :components ((:file "fixed")
                             (:file "linear")
                             (:file "grid")
                             (:file "border")))
               (:module "components"
                :components ((:file "base")
                             (:file "button")
                             (:file "switch")
                             (:file "text-input")
                             (:file "slider")
                             (:file "radio")
                             (:file "combo")))
               (:file "documentation"))
  :depends-on (:documentation-utils
               :array-utils)
  :in-order-to ((asdf:test-op (asdf:test-op :alloy-test))))
