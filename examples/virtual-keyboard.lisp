(in-package #:org.shirakumo.alloy.examples)

(define-example virtual-keyboard (screen)
  "A window containing Alloy's virtual keyboard."
  (let* ((window (windowing:make-window screen))
         (focus (make-instance 'alloy:focus-list :focus-parent window))
         (layout (make-instance 'alloy:vertical-linear-layout :layout-parent window))
         (text "Hello")
         (line (alloy:represent text 'alloy:input-line))
         (keyboard (make-instance 'alloy::virtual-keyboard :target line)))
    (alloy:enter-all focus line keyboard)
    (alloy:enter-all layout line keyboard)))
