#|
 This file is a part of Alloy
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.alloy)

(defclass text-input-component (interactable-component text-component)
  ((cursor :initform 0 :accessor cursor)
   (text :initform (make-array 0 :adjustable T :fill-pointer T :element-type 'character))))

(defmethod handle ((event text-event) (component text-input-component) ctx)
  (loop for char across (text event)
        do (array-utils:vector-push-extend-position char (text component) (cursor component))
           (incf (cursor component))))

(defmethod handle ((event key-up) (component text-input-component) ctx)
  ;; TODO: insert mode
  ;; TODO: selection
  ;; TODO: key repeats
  ;; TODO: copy
  (case (key event)
    (:escape
     (exit (focus-element component ctx)))
    (:backspace
     (when (< 0 (cursor component))
       (decf (cursor component))
       (array-utils:vector-pop-position (text component) (cursor component))))
    (:delete
     (when (< (cursor component) (length (text component)))
       (array-utils:vector-pop-position (text component) (cursor component))))
    (:left
     (when (< 0 (cursor component))
       (decf (cursor component))))
    (:right
     (when (< (cursor component) (length (text component)))
       (incf (cursor component))))
    (:home
     (setf (cursor component) 0))
    (:end
     (setf (cursor component) (length (text component))))
    (T
     (call-next-method))))

(defmethod handle ((event paste-event) (component text-input-component) ctx)
  (let ((content (content event))
        (text (text component)))
    (typecase content
      (string
       (when (< (+ (length text) (length content))
                (array-total-size text))
         (adjust-array text (+ (length text) (length content)) :fill-pointer T)
         (replace text content :start1 (- (length text) (length content)))))
      (T
       (call-next-method)))))

(defclass input-line (text-input-component)
  ())

(defmethod handle ((event key-up) (component input-line) ctx)
  (case (key event)
    (:return
      (exit (focus-element component ctx)))
    (T
     (call-next-method))))

(defmethod handle ((event paste-event) (component text-input-component) ctx)
  ;; Override to leave out returns and linefeeds.
  (let ((content (content event))
        (text (text component)))
    (typecase content
      (string
       (setf content (remove-if (lambda (c) (find c '(#\Return #\Linefeed))) content))
       (when (< (+ (length text) (length content))
                (array-total-size text))
         (adjust-array text (+ (length text) (length content)) :fill-pointer T)
         (replace text content :start1 (- (length text) (length content)))))
      (T
       (call-next-method)))))

(defclass input-box (text-input-component)
  ())

(defmethod handle ((event key-up) (component input-box) ctx)
  (case (key event)
    (:return
      (vector-push-extend #\Linefeed (text component)))
    (T
     (call-next-method))))
