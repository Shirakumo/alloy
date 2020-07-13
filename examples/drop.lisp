#|
 This file is a part of Alloy
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.alloy.examples)

(defclass custom-window (glfw:window)
  ())

(alloy:define-observable show-drop (paths alloy:observable))

(defmethod alloy:handle ((event alloy:drop-event) (window custom-window))
  (alloy:notify-observers 'show-drop window (alloy:paths event) window))

(define-example drop-files (screen)
  (let* ((window (windowing:make-window screen :class 'custom-window))
         (focus (make-instance 'alloy:focus-list :focus-parent window))
         (layout (make-instance 'alloy:border-layout :layout-parent window))
         (label (alloy:represent "Drop some files here!" 'alloy:button :focus-parent focus :layout-parent layout)))
    (alloy:on show-drop (paths window)
      (setf (alloy:value label) (princ-to-string paths)))))
