#|
 This file is a part of Alloy
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.alloy)

(defclass text-input-component (value-component)
  ((insert-mode :initform :add :initarg :insert-mode :accessor insert-mode)
   ;; TODO: Maybe make cursors multiple?
   (cursor :initform 0 :accessor cursor)))

(defmethod (setf cursor) :after (value (component text-input-component))
  (mark-for-render component))

(defun maybe-enlarge (array size)
  (if (< (array-total-size array) size)
      (adjust-array array size :fill-pointer size)
      (setf (fill-pointer array) size)))

(defmethod insert-text (text (component text-input-component))
  (let ((old (value component))
        (cursor (cursor component)))
    ;; Now either add by shifting or replace (and extend).
    (unless (adjustable-array-p old)
      (setf old (make-array (length old) :element-type 'character :adjustable T :fill-pointer T :initial-contents old)))
    (ecase (insert-mode component)
      (:add
       (maybe-enlarge old (+ (length old) (length text)))
       (loop for i downfrom (1- (fill-pointer old)) above cursor
             do (setf (aref old i) (aref old (- i (length text)))))
       (replace old text :start1 cursor))
      (:replace
       (maybe-enlarge old (max (length old) (+ cursor (length text))))
       (replace old text :start1 cursor)))
    (setf (value component) old)
    (setf (cursor component) (+ cursor (length text)))))

(defmethod handle ((event text-event) (component text-input-component) ctx)
  (insert-text (text event) component))

(defmethod handle ((event key-up) (component text-input-component) ctx)
  ;; TODO: selection
  ;; TODO: key repeats
  ;; TODO: copy
  (case (key event)
    (:escape
     (exit component))
    (:backspace
     (when (< 0 (cursor component))
       (decf (cursor component))
       (array-utils:vector-pop-position (value component) (cursor component))
       (refresh component)))
    (:delete
     (when (< (cursor component) (length (value component)))
       (array-utils:vector-pop-position (value component) (cursor component))
       (refresh component)))
    (:left
     (when (< 0 (cursor component))
       (decf (cursor component))))
    (:right
     (when (< (cursor component) (length (value component)))
       (incf (cursor component))))
    (:home
     (setf (cursor component) 0))
    (:end
     (setf (cursor component) (length (value component))))
    (:insert
     (setf (insert-mode component)
           (ecase (insert-mode component)
             (:replace :add)
             (:add :replace))))
    (T
     (call-next-method))))

(defmethod handle ((event paste-event) (component text-input-component) ctx)
  (let ((content (content event)))
    (typecase content
      (string
       (insert-text content component))
      (T
       (call-next-method)))))

(defclass input-line (text-input-component)
  ())

(defmethod handle ((event key-up) (component input-line) ctx)
  (case (key event)
    (:return
      (exit component))
    (T
     (call-next-method))))

(defmethod handle ((event paste-event) (component text-input-component) ctx)
  ;; Override to leave out returns and linefeeds.
  (let ((content (content event)))
    (typecase content
      (string
       (insert-text (remove-if (lambda (c) (find c '(#\Return #\Linefeed))) content) component))
      (T
       (call-next-method)))))

(defclass input-box (text-input-component)
  ())

(defmethod handle ((event key-up) (component input-box) ctx)
  (case (key event)
    (:return
      (insert-text (string #\Linefeed) component))
    (T
     (call-next-method))))
