#|
 This file is a part of Alloy
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.alloy)

(defclass text-component (component)
  ((text :initarg :text :initform "" :accessor text)))

(defclass image-component (component)
  ((image :initarg :image :initform NIL :accessor image)))

(defclass label (text-component)
  ())

(defclass icon (image-component)
  ())

(defclass button (text-component image-component)
  ((pressed :initform NIL :accessor pressed)))

(defmethod handle ((event pointer-down) (button button) ctx)
  (setf (pressed button) T))

(defmethod handle ((event pointer-up) (button button) ctx)
  (setf (pressed button) NIL))

(defmethod handle ((event button-down) (button button) ctx)
  (case (button event)
    (:a (setf (pressed button) T))
    (T (call-next-method))))

(defmethod handle ((event button-up) (button button) ctx)
  (case (button event)
    (:a (setf (pressed button) NIL))
    (T (call-next-method))))

(defmethod (setf focus) :after (focus (button button))
  (when (eql NIL focus) (setf (pressed button) NIL)))

(defclass switch ()
  ((state :initarg :state :initform NIL :accessor state)))

(defmethod activate ((switch switch))
  (setf (state switch) (not (state switch))))

;; TODO: slider
(defclass slider ()
  ())

(defclass text-input-component (text-component)
  ((cursor :initform 0 :accessor cursor)
   (text :initform (make-array 0 :adjustable T :fill-pointer T))))

(defmethod handle ((event text-event) (component text-input-component) ctx)
  (loop for char across (text event)
        do (vector-push-extend char (text component))))

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
     (when (< (cursor component) (1- (length (text component))))
       (array-utils:vector-pop-position (text component) (cursor component))))
    (:left
     (when (< 0 (cursor component))
       (decf (cursor component))))
    (:right
     (when (< (cursor component) (1- (length (text component))))
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

(defmethod handle ((event key-up) (component input-box) ctx)
  (case (key event)
    (:return
      (exit (focus-element component ctx)))
    (T
     (call-next-method))))

(defmethod handle ((event paste-event) (component text-input-component) ctx)
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
