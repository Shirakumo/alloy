(in-package #:org.shirakumo.alloy.examples)

(defclass custom-window (glfw:window)
  ((label :reader label)))

(defmethod alloy:handle ((ev alloy:drop-event) (window custom-window))
  (when (typep ev 'alloy:drop-event)
    (setf (alloy:value (label window)) (princ-to-string (alloy:paths ev)))))

(define-example drop-files (screen)
  (let* ((window (windowing:make-window screen :class 'custom-window))
         (focus (make-instance 'alloy:focus-list :focus-parent window))
         (layout (make-instance 'alloy:border-layout :layout-parent window))
         (label (alloy:represent "Drop some files here!" 'alloy:button :focus-parent focus :layout-parent layout)))
    (setf (slot-value window 'label) label)))
