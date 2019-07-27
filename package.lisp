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
   #:focus-tree))
