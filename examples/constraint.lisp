#|
 This file is a part of Alloy
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.alloy.examples)

(define-example constraint-layout (screen)
  (let* ((window (windowing:make-window screen))
         (focus (make-instance 'alloy:focus-list :focus-parent window))
         (layout (make-instance 'org.shirakumo.alloy.layouts.constraint:layout :layout-parent window))
         (button (alloy:represent "Greet" 'alloy:button)))
    (alloy:enter button layout :constraints '((:left 0) (:top 0) (:width 100) (:height 100)))
    (alloy:enter button focus)))
