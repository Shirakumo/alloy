#|
 This file is a part of Alloy
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage #:org.shirakumo.alloy
  (:use #:cl)
  (:shadow #:step #:structure #:close #:declaim #:proclaim)
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
   #:place-already-occupied
   #:place
   #:existing
   #:allocation-failed
   #:renderer)
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
  ;; components/plot.lisp
  (:export
   #:plot
   #:x-range
   #:y-range
   #:plot-points)
  ;; components/radio.lisp
  (:export
   #:radio
   #:active-value
   #:active-p)
  ;; components/switch.lisp
  (:export
   #:switch)
  ;; components/text-input.lisp
  (:export
   #:text-input-component
   #:insert-mode
   #:cursor
   #:pos
   #:anchor
   #:insert-text
   #:input-line
   #:accept
   #:input-box)
  ;; components/scroll.lisp
  (:export
   #:scrollbar
   #:x-scrollbar
   #:y-scrollbar)
  ;; components/slider.lisp
  (:export
   #:slider
   #:range
   #:step
   #:state
   #:orientation
   #:minimum
   #:maximum
   #:slider-unit
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
   #:index-element
   #:element-index
   #:clear
   #:vector-container
   #:stack-container
   #:layers)
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
   #:index
   #:computed-data
   #:closure)
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
   #:dx
   #:dy
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
   #:overlapping-p
   #:ensure-extent
   #:extent-intersection
   #:destructure-extent)
  ;; layout.lisp
  (:export
   #:layout-tree
   #:bounds
   #:layout-element
   #:layout-parent
   #:notice-bounds
   #:suggest-bounds
   #:ensure-visible
   #:layout-element
   #:layout
   #:layout-tree)
  ;; layouts/border.lisp
  (:export
   #:border-layout)
  ;; layouts/clip-view.lisp
  (:export
   #:clip-view
   #:offset
   #:stretch)
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
  ;; layouts/swap.lisp
  (:export
   #:swap-layout
   #:index
   #:current)
  ;; observable.lisp
  (:export
   #:observable
   #:observe
   #:remove-observers
   #:list-observers
   #:notify-observers
   #:make-observable
   #:define-observable
   #:on
   #:observable-object)
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
   #:call-with-constrained-visibility
   #:with-constrained-visibility
   #:extent-visible-p
   #:renderer
   #:renderable)
  ;; structure.lisp
  (:export
   #:structure
   #:layout-element
   #:focus-element
   #:finish-structure)
  ;; structures/query.lisp
  (:export
   #:query)
  ;; structures/scroll-view.lisp
  (:export
   #:scroll-view)
  ;; structures/tab-view.lisp
  (:export
   #:tab
   #:name
   #:tab-view
   #:tab-button
   #:index)
  ;; structures/window.lisp
  (:export
   #:window
   #:close
   #:minimize
   #:maximize)
  ;; ui.lisp
  (:export
   #:clipboard
   #:ui
   #:layout-tree
   #:focus-tree
   #:dots-per-cm
   #:target-resolution
   #:resolution-scale
   #:base-scale)
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
