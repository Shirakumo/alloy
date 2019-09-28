#|
 This file is a part of Alloy
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.alloy)

(defclass cursor ()
  ((pos :initform 0 :reader pos :writer set-pos)
   (anchor :initform NIL :reader anchor :writer set-anchor)
   (component :initarg :component :initform (arg! :component) :reader component)))

(defmethod (setf pos) ((position integer) (cursor cursor))
  (set-pos (max 0 (min position (length (value (component cursor))))) cursor))

(defmethod (setf anchor) ((position integer) (cursor cursor))
  (set-anchor (max 0 (min position (length (value (component cursor))))) cursor))

(defmethod (setf anchor) ((null null) (cursor cursor))
  (set-anchor null cursor))

(defmethod move-to ((_ (eql :start)) (cursor cursor))
  (set-pos 0 cursor))

(defmethod move-to ((_ (eql :end)) (cursor cursor))
  (set-pos (length (value (component cursor))) cursor))

(defmethod move-to ((_ (eql :prev-char)) (cursor cursor))
  (set-pos (max 0 (1- (pos cursor))) cursor))

(defmethod move-to ((_ (eql :next-char)) (cursor cursor))
  (set-pos (min (length (value (component cursor))) (1+ (pos cursor))) cursor))

(defmethod move-to ((_ (eql :prev-line)) (cursor cursor))
  (let ((pos (pos cursor)))
    (move-to :line-start cursor)
    (when (< 0 (pos cursor))
      (let ((line-end (pos cursor))
            (col (- (pos cursor) pos)))
        (set-pos (1- (pos cursor)) cursor)
        (move-to :line-start cursor)
        (set-pos (min (1- line-end) (+ (pos cursor) col)) cursor)))))

(defmethod move-to ((_ (eql :next-line)) (cursor cursor))
  (let ((pos (pos cursor)))
    (move-to :line-start cursor)
    (let ((col (- (pos cursor) pos)))
      (move-to :line-end cursor)
      (when (< (pos cursor) (length (value (component cursor))))
        (let ((start (1+ (pos cursor))))
          (move-to :line-end cursor)
          (set-pos (min (+ start col) (pos cursor)) cursor))))))

(defmethod move-to ((_ (eql :line-start)) (cursor cursor))
  (let ((string (value (component cursor))))
    (set-pos (loop for i downfrom (pos cursor) above 0
                   do (when (char= #\Linefeed (char string (1- i)))
                        (return i))
                   finally (return 0))
             cursor)))

(defmethod move-to ((_ (eql :line-end)) (cursor cursor))
  (let ((string (value (component cursor))))
    (set-pos (loop for i from (pos cursor) below (length string)
                   do (when (char= #\Linefeed (char string i))
                        (return i))
                   finally (return (length string)))
             cursor)))

(defmethod move-to ((position integer) (cursor cursor))
  (set-pos (max 0 (min position (length (value (component cursor))))) cursor))

(defclass text-input-component (value-component)
  ((insert-mode :initform :add :initarg :insert-mode :accessor insert-mode)
   ;; TODO: Maybe make cursors multiple?
   (cursor :reader cursor)))

(defmethod initialize-instance :after ((component text-input-component) &key)
  (setf (slot-value component 'cursor) (make-instance 'cursor :component component)))

(defun maybe-enlarge (array size)
  (if (< (array-total-size array) size)
      (adjust-array array size :fill-pointer size)
      (setf (fill-pointer array) size)))

(defmethod insert-text (text (component text-input-component))
  (let ((old (value component))
        (cursor (pos (cursor component))))
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
    (move-to (+ cursor (length text)) (cursor component))))

(defmethod handle ((event text-event) (component text-input-component) ctx)
  (insert-text (text event) component))

(defmethod handle ((event key-up) (component text-input-component) ctx)
  ;; TODO: selection
  ;; TODO: copy
  (case (key event)
    (:escape
     (exit component))
    (:backspace
     (when (< 0 (pos (cursor component)))
       (move-to :prev-char (cursor component))
       (array-utils:vector-pop-position (value component) (pos (cursor component)))
       (refresh component)))
    (:delete
     (when (< (pos (cursor component)) (length (value component)))
       (array-utils:vector-pop-position (value component) (pos (cursor component)))
       (refresh component)))
    (:left
     (move-to :prev-char (cursor component)))
    (:right
     (move-to :next-char (cursor component)))
    (:home
     (move-to :start (cursor component)))
    (:end
     (move-to :end (cursor component)))
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
