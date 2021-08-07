#|
 This file is a part of Alloy
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.alloy.examples)

(define-example gradient (screen)
  (let* ((window (windowing:make-window screen))
         (focus (make-instance 'alloy:focus-list :focus-parent window))
         (layout (make-instance 'alloy:vertical-linear-layout :layout-parent window))
         (linear (simple:request-gradient screen 'simple:linear-gradient (alloy:px-point 0 0) (alloy:px-point 100 0)
                                          #((0.0 #.colors:red)
                                            (1.0 #.colors:green)))))
    (alloy:enter (make-instance 'alloy:component :shapes (list (simple:rectangle screen (alloy:margins) :pattern linear))
                                                 :data NIL
                                                 :ideal-bounds (alloy:extent 0 0 100 100))
                 layout)))
