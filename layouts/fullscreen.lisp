#|
 This file is a part of Alloy
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.alloy)

(defclass fullscreen-layout (layout vector-container)
  ())

(defmethod notice-size ((element layout-element) (layout fullscreen-layout))
  (setf (bounds element) (bounds layout)))

(defmethod suggest-size (size (layout fullscreen-layout))
  (let ((w (w size))
        (h (h size)))
    (loop for element across (elements layout)
          for new = (suggest-size size element)
          do (setf w (umax w (w new)))
             (setf h (umax h (h new))))
    (size w h)))

(defmethod (setf bounds) :after (extent (layout fullscreen-layout))
  (loop for element across (elements layout)
        do (resize element (w layout) (h layout))))

(defmethod render ((renderer renderer) (layout fullscreen-layout))
  (loop for element across (elements layout)
        do (render renderer element)))

(defmethod maybe-render ((renderer renderer) (layout fullscreen-layout))
  (loop for element across (elements layout)
        do (maybe-render renderer element)))

(defmethod ensure-visible ((element layout-element) (layout fullscreen-layout))
  (call-next-method))

(defmethod handle ((event pointer-event) (layout fullscreen-layout))
  (loop for i downfrom (1- (length (elements layout))) to 0
        for element = (aref (elements layout) i)
        do (when (handle event element)
             (return))
        finally (decline)))
