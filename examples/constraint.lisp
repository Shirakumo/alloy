(in-package #:org.shirakumo.alloy.examples)

(define-example constraint-layout (screen)
  "A simple demonstration of the constraint-based layout."
  (let* ((window (windowing:make-window screen))
         (focus (make-instance 'alloy:focus-list :focus-parent window))
         (layout (make-instance 'org.shirakumo.alloy.layouts.constraint:layout :layout-parent window))
         (button (alloy:represent "Greet" 'alloy:button))
         (button2 (alloy:represent "Hi" 'alloy:button)))
    (alloy:enter button layout :constraints '(:contained (:left 0) (:top 0) (:width 100) (:height 100)))
    (alloy:enter button2 layout :constraints `((:chain :right ,button 10) (:width 100)))
    (alloy:enter button focus)))
