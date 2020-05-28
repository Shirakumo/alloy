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
         (button (alloy:represent "Greet" 'alloy:button)))
    (alloy:enter button layout)
    (alloy:enter button focus)
    (alloy:on alloy:activate (button)
      (let ((window (windowing:make-window screen :background-color colors:white)))
        (alloy:enter (alloy:represent "Hey!" T) window)))))
