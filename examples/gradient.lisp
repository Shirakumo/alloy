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
         (linear (simple:request-gradient screen 'simple:linear-gradient (alloy:point 0 0) (alloy:point (alloy:pw 1) 0)
                                          #((0.5 #.colors:white)
                                            (1.0 #.colors:black)))))
    (alloy:enter (make-instance 'alloy:component :shapes (list (simple:rectangle screen (alloy:margins) :pattern linear))
                                                 :data NIL
                                                 :ideal-size (alloy:size 100 100))
                 layout)))
