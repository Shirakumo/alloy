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
               (:file "container")
               (:file "focus-tree")
               (:file "layout")
               (:file "component")
               (:file "structure")
               (:file "ui")
               (:module "layouts"
                :components ((:file "fixed")
                             (:file "linear")
                             (:file "grid")
                             (:file "border")
                             (:file "clip-view")
                             (:file "swap")
                             (:file "popup")))
               (:module "components"
                :components ((:file "base")
                             (:file "button")
                             (:file "switch")
                             (:file "text-input")
                             (:file "slider")
                             (:file "radio")
                             (:file "combo")
                             (:file "scroll")
                             (:file "plot")
                             (:file "drag")))
               (:module "structures"
                :components ((:file "query")
                             (:file "scroll-view")
                             (:file "tab-view")
                             (:file "window")))
               (:file "builder")
               (:file "widget")
               (:file "documentation"))
  :depends-on (:documentation-utils
               :array-utils
               :closer-mop)
  :in-order-to ((asdf:test-op (asdf:test-op :alloy-test))))
