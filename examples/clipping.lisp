(in-package #:org.shirakumo.alloy.examples)

(define-example clipping (screen)
  (let* ((window (windowing:make-window screen :min-size (alloy:px-size 640 480)))
         (clip (make-instance 'alloy:clip-view :layout-parent window))
         (layout (make-instance 'alloy:vertical-linear-layout :layout-parent clip)))
    (dotimes (i 5)
      (alloy:enter "Group" layout)
      (let ((layout (make-instance 'alloy:grid-layout :col-sizes '(200 T) :row-sizes '(50)
                                                      :layout-parent layout)))
        (dotimes (i 20)
          (alloy:enter "Item" layout))))))