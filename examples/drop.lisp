#|
 This file is a part of Alloy
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.alloy.examples)

;; KLUDGE: This is necessary to add the below handle method that blindly consumes
;;         the drop event. By default the event is declined, which causes the
;;         observers on HANDLE to not get triggered. I'm not sure if it would be
;;         correct to be able to observe declined events or not, so I don't know
;;         what the correct solution is here. I don't like that you have to create
;;         an extra class just for this, though.
(defclass custom-window (glfw:window)
  ())

(defmethod alloy:handle ((event alloy:drop-event) (window custom-window)))

(define-example drop-files (screen)
  (let* ((window (windowing:make-window screen :class 'custom-window))
         (focus (make-instance 'alloy:focus-list :focus-parent window))
         (layout (make-instance 'alloy:border-layout :layout-parent window))
         (label (alloy:represent "Drop some files here!" 'alloy:button :focus-parent focus :layout-parent layout)))
    (alloy:on alloy:handle (ev window)
      (when (typep ev 'alloy:drop-event)
        (setf (alloy:value label) (princ-to-string (alloy:paths ev)))))))
