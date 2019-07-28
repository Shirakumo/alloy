#|
 This file is a part of Alloy
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage #:org.shirakumo.alloy
  (:use #:cl)
  ;; component.lisp
  (:export
   #:component)
  ;; container.lisp
  (:export
   #:element
   #:container
   #:enter
   #:leave
   #:update
   #:elements
   #:call-with-elements
   #:do-elements
   #:vector-container
   #:element-table
   #:associate
   #:disassociate
   #:associated-element)
  ;; extent.lisp
  (:export
   #:x
   #:y
   #:w
   #:h
   #:extent
   #:make-extent
   #:copy-extent
   #:extent-p
   #:destructure-extent
   #:with-extent)
  ;; focus-tree.lisp
  (:export
   #:focus-element
   #:focus-tree
   #:parent
   #:focus
   #:exit
   #:activate
   #:handle
   #:notice-focus
   #:index
   #:focused
   #:element-index
   #:focus-next
   #:focus-prev
   #:focus-up
   #:focus-down
   #:root
   #:focus-element
   #:focus-entry
   #:focus-chain
   #:focus-list
   #:focus-grid
   #:focus-tree)
  ;; layout.lisp
  (:export
   #:layout-tree
   #:extent
   #:layout-element
   #:notice-extent
   #:layout-element
   #:extent
   #:layout-entry
   #:component
   #:layout
   #:layout-tree)
  ;; renderer
  (:export
   #:allocate
   #:deallocate
   #:register
   #:render-needed-p
   #:mark-for-render
   #:render
   #:maybe-render
   #:renderer
   #:renderable)
  ;; ui.lisp
  (:export
   #:extent-for
   #:focus-for
   #:ui
   #:layout-tree
   #:focus-tree))
