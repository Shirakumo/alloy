(in-package #:org.shirakumo.alloy.examples)

(defclass animated-button (alloy:button) ())

(presentations:define-animated-shapes animated-button
  (:background
   (simple:pattern :duration 1.0)))

(define-example animation (screen)
  "Simple animated transition between button states."
  (let* ((window (windowing:make-window screen))
         (focus (make-instance 'alloy:focus-list :focus-parent window))
         (layout (make-instance 'alloy:vertical-linear-layout :layout-parent window))
         (a (alloy:represent "Hello" 'animated-button))
         (b (alloy:represent "There" 'animated-button)))
    (alloy:enter-all focus a b)
    (alloy:enter-all layout a b)))
