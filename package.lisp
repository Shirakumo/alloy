#|
 This file is a part of Alloy
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage #:org.shirakumo.alloy
  (:use #:cl)
  (:shadow #:step)
  ;; component.lisp
  (:export
   #:component
   #:component-class-for-object
   #:represent-with
   #:represent)
  ;; conditions.lisp
  (:export
   #:alloy-condition
   #:define-alloy-condition
   #:argument-missing
   #:initarg
   #:arg!
   #:index-out-of-range
   #:index
   #:range
   #:hierarchy-error
   #:container
   #:element
   #:element-has-different-parent
   #:element-not-contained
   #:element-has-different-root
   #:root-already-established
   #:layout-condition
   #:layout
   #:renderer-condition
   #:renderer
   #:renderable-already-registered
   #:place-already-occupied
   #:place
   #:existing)
  ;; components/base.lisp
  (:export
   #:label
   #:icon
   #:value-component
   #:direct-value-component
   #:progress)
  ;; components/button.lisp
  (:export
   #:button
   #:pressed)
  ;; components/combo.lisp
  (:export
   #:combo-item
   #:combo
   #:combo-list
   #:combo-item
   #:value-set
   #:combo-set)
  ;; components/radio.lisp
  (:export
   #:radio
   #:active-p)
  ;; components/switch.lisp
  (:export
   #:switch)
  ;; components/text-input.lisp
  (:export
   #:text-input-component
   #:insert-mode
   #:cursor
   #:insert-text
   #:input-line
   #:input-box)
  ;; components/slider.lisp
  (:export
   #:slider
   #:range
   #:step
   #:state
   #:minimum
   #:maximum
   #:ranged-slider)
  ;; container.lisp
  (:export
   #:element
   #:container
   #:enter
   #:leave
   #:update
   #:elements
   #:element-count
   #:call-with-elements
   #:do-elements
   #:clear
   #:vector-container)
  ;; data.lisp
  (:export
   #:data
   #:refresh
   #:expand-place-data
   #:expand-compound-place-data
   #:value-data
   #:value
   #:place-data
   #:getter
   #:setter
   #:slot-data
   #:object
   #:slot
   #:aref-data
   #:object
   #:index)
  ;; events.lisp
  (:export
   #:handle
   #:event
   #:decline
   #:pointer-event
   #:location
   #:pointer-move
   #:old-location
   #:pointer-down
   #:kind
   #:pointer-up
   #:kind
   #:scroll
   #:delta
   #:direct-event
   #:paste-event
   #:content
   #:text-event
   #:text
   #:key-event
   #:key
   #:code
   #:key-down
   #:key-up
   #:button-event
   #:button
   #:device
   #:button-down
   #:button-up
   #:focus-event
   #:focus-next
   #:focus-prev
   #:focus-up
   #:focus-down
   #:activate
   #:exit)
  ;; focus-tree.lisp
  (:export
   #:focus-element
   #:focus-tree
   #:focus-parent
   #:focus
   #:exit
   #:activate
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
   #:focus-chain
   #:focus-list
   #:focus-grid
   #:focus-tree)
  ;; geometry.lisp
  (:export
   #:x
   #:y
   #:w
   #:h
   #:l
   #:u
   #:r
   #:b
   #:contained-p
   #:pxx
   #:pxy
   #:pxw
   #:pxh
   #:pxl
   #:pxu
   #:pxr
   #:pxb
   #:point
   #:px-point
   #:point-p
   #:point-x
   #:point-y
   #:point=
   #:size
   #:px-size
   #:size-p
   #:size-w
   #:size-h
   #:size=
   #:margins
   #:margins-p
   #:margins-l
   #:margins-u
   #:margins-r
   #:margins-b
   #:margins=
   #:extent
   #:px-extent
   #:extent-p
   #:extent-x
   #:extent-y
   #:extent-w
   #:extent-h
   #:extent=
   #:destructure-extent)
  ;; layout.lisp
  (:export
   #:layout-tree
   #:bounds
   #:layout-element
   #:layout-parent
   #:notice-bounds
   #:suggest-bounds
   #:layout-element
   #:layout
   #:layout-tree)
  ;; layouts/border.lisp
  (:export
   #:border-layout)
  ;; layouts/fixed.lisp
  (:export
   #:fixed-layout)
  ;; layouts/grid.lisp
  (:export
   #:grid-layout
   #:row-sizes
   #:col-sizes
   #:stretch
   #:row
   #:col)
  ;; layouts/linear.lisp
  (:export
   #:linear-layout
   #:min-size
   #:stretch
   #:align
   #:vertical-linear-layout
   #:horizontal-linear-layout)
  ;; observable.lisp
  (:export
   #:observable
   #:observe
   #:remove-observers
   #:list-observers
   #:notify-observers
   #:make-observable
   #:define-observable
   #:on)
  ;; renderer.lisp
  (:export
   #:allocate
   #:allocated-p
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
   #:clipboard
   #:cursor
   #:ui
   #:layout-tree
   #:focus-tree)
  ;; units.lisp
  (:export
   #:with-unit-parent
   #:unit
   #:to-px
   #:to-un
   #:define-unit
   #:px
   #:vw
   #:vh
   #:pw
   #:ph
   #:un
   #:cm
   #:u+
   #:u*
   #:u-
   #:u/
   #:u=
   #:u/=
   #:u<
   #:u>
   #:u<=
   #:u>=))
