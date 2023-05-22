#|
 This file is a part of Alloy
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.alloy)

;;; Provided by the backend
(defgeneric clipboard (ui))
(defgeneric (setf clipboard) (content ui))
;; Standard cursors: :ARROW :TEXT :HAND :CROSSHAIR :RESIZE :DISALLOWED
(defgeneric cursor (ui))
(defgeneric (setf cursor) (cursor ui))
(defgeneric key-text (key ui))

(defclass ui ()
  ((layout-tree :initarg :layout-tree :reader layout-tree)
   (focus-tree :initarg :focus-tree :reader focus-tree)
   ;; 38dpcm corresponds to Windows' default 96dpi
   (dots-per-cm :initform 38 :reader dots-per-cm)
   (target-resolution :initarg :target-resolution :initform (px-size 1920 1080) :accessor target-resolution)
   (resolution-scale :initform 1.0 :accessor resolution-scale)
   (base-scale :initform 1.0 :accessor base-scale))
  (:default-initargs
   :layout-tree (make-instance 'layout-tree)
   :focus-tree (make-instance 'focus-tree)))

(defmethod shared-initialize :after ((ui ui) slots &key base-scale)
  (unless (slot-boundp (layout-tree ui) 'ui)
    (setf (slot-value (layout-tree ui) 'ui) ui))
  (when base-scale (setf (slot-value ui 'base-scale) (float base-scale 0f0))))

(defmethod w ((ui ui)) (w (layout-tree ui)))
(defmethod h ((ui ui)) (h (layout-tree ui)))

(defmethod clear ((ui ui))
  (clear (layout-tree ui))
  (clear (focus-tree ui)))

(defmethod focused ((ui ui))
  (focused (focus-tree ui)))

(defmethod handle ((event direct-event) (ui ui))
  (or (handle event (focus-tree ui))
      (decline)))

(defmethod handle ((event layout-event) (ui ui))
  (or (handle event (focus-tree ui))
      (handle event (layout-tree ui))
      (progn
        (setf (cursor ui) :arrow)
        (decline))))

(defmethod render ((renderer renderer) (ui ui))
  (render renderer (layout-tree ui)))

(defmethod maybe-render ((renderer renderer) (ui ui))
  (maybe-render renderer (layout-tree ui)))

(defmethod activate ((ui ui))
  (mark-for-render (root (layout-tree ui))))

(defmethod bounds ((ui ui))
  (bounds (root (layout-tree ui))))

(defmethod suggest-size (size (ui ui))
  (suggest-size size (layout-tree ui)))

(defmethod register ((ui ui) renderer)
  (register (layout-tree ui) renderer))

(defmethod deregister ((ui ui) renderer)
  (deregister (layout-tree ui) renderer))

(defmethod (setf base-scale) (value (ui ui))
  (setf (slot-value ui 'base-scale) (float value 0f0))
  (handle (make-instance 'scale-changed) ui))

(defmethod (setf resolution-scale) (value (ui ui))
  (setf (slot-value ui 'resolution-scale) (float value 0f0))
  (handle (make-instance 'scale-changed) ui))

(defmethod (setf base-scale) :after (value (ui ui))
  (suggest-size (bounds ui) ui))

(defmethod refresh ((ui ui))
  (refresh (layout-tree ui)))

(defclass smooth-scaling-ui (ui)
  ())

(defmethod suggest-size (size (ui smooth-scaling-ui))
  (let* ((target (target-resolution ui))
         (wr (/ (pxw size) (pxw target)))
         (hr (/ (pxh size) (pxh target))))
    (setf (resolution-scale ui) (min wr hr)))
  (call-next-method))

(defclass fixed-scaling-ui (ui)
  ((scales :initform '((T T 1.0)) :initarg :scales :accessor scales)))

(defmethod suggest-size (size (ui fixed-scaling-ui))
  (setf (resolution-scale ui)
        (loop for (w h scale) in (scales ui)
              do (when (and (or (eql T w) (<= w (pxw size)))
                            (or (eql T h) (<= h (pxh size))))
                   (return scale))
              finally (return 1.0)))
  (call-next-method))

(defclass lock-step-scaling-ui (ui)
  ((scale-step :initform 0.5 :initarg :scale-step :accessor scale-step)
   (scale-direction :initform :down :initarg :scale-direction :accessor scale-direction)))

(defmethod suggest-size (size (ui lock-step-scaling-ui))
  (let ((target (target-resolution ui))
        (step (scale-step ui)))
    (flet ((factor (ratio)
             (ecase (scale-direction ui)
               (:down
                (if (< step ratio)
                    (* step (floor ratio step))
                    (/ step (ceiling step ratio))))
               (:closest
                (if (< step ratio)
                    (* step (round ratio step))
                    (/ step (round step ratio))))
               (:up
                (if (< step ratio)
                    (* step (ceiling ratio step))
                    (/ step (floor step ratio)))))))
      (let ((wr (factor (/ (pxw size) (pxw target))))
            (hr (factor (/ (pxh size) (pxh target)))))
        (setf (resolution-scale ui) (min wr hr)))))
  (call-next-method))
