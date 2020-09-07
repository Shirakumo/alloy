#|
 This file is a part of Alloy
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.alloy.examples)

(define-example simple-window (screen)
  (let* ((window (windowing:make-window screen))
         (focus (make-instance 'alloy:focus-list :focus-parent window))
         (layout (make-instance 'alloy:vertical-linear-layout :layout-parent window))
         (name "")
         (button (alloy:represent "Greet" 'alloy:button)))
    (alloy:represent name 'alloy:input-line :focus-parent focus :layout-parent layout)
    (alloy:enter button layout)
    (alloy:enter button focus)
    (alloy:on alloy:activate (button)
      (let ((window (windowing:make-window screen :background-color colors:white)))
        (alloy:enter (make-instance 'alloy:label* :value (format NIL "Hello, ~a!" name)) window)))))

(define-example combo (screen)
  (let* ((window (windowing:make-window screen))
         (focus (make-instance 'alloy:focus-list :focus-parent window))
         (layout (make-instance 'alloy:vertical-linear-layout :layout-parent window))
         (selection NIL)
         (value 0f0)
         (combo (alloy:represent selection 'alloy:combo-set :value-set '(NIL :a :b :c)))
         (slider (alloy:represent value 'alloy:ranged-slider))
         (button (alloy:represent "Confirm" 'alloy:button)))
    (alloy:enter combo layout)
    (alloy:enter slider layout)
    (alloy:enter button layout)
    (alloy:enter combo focus)
    (alloy:enter slider focus)
    (alloy:enter button focus)))

(define-example bag-grid (screen)
  (let* ((window (windowing:make-window screen))
         (focus (make-instance 'alloy:focus-list :focus-parent window))
         (layout (make-instance 'alloy:grid-bag-layout
                                :layout-parent window
                                :col-sizes (make-array 3 :initial-contents '(50 100 50))
                                :row-sizes (make-array 3 :initial-contents '(50 100 50))))
         (but1 (alloy:represent "One" 'alloy:button :focus-parent focus))
         (but2 (alloy:represent "Two" 'alloy:button :focus-parent focus))
         (but3 (alloy:represent "Three" 'alloy:button :focus-parent focus)))
    (alloy:enter but1 layout :col 0 :row 0 :width 3 :height 1)
    (alloy:enter but2 layout :col 0 :row 1 :height 2 :width 1)
    (alloy:enter but3 layout :col 1 :row 1 :width 2 :height 2)))

