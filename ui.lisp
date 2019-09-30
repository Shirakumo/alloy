#|
 This file is a part of Alloy
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.alloy)

;;; Provided by the backend
(defgeneric clipboard (ui))
(defgeneric (setf clipboard) (content ui))
(defgeneric cursor (ui))
(defgeneric (setf cursor) (cursor ui))

(defclass ui (renderer)
  ((layout-tree :initarg :layout-tree :reader layout-tree)
   (focus-tree :initarg :focus-tree :reader focus-tree)
   ;; 38dpcm corresponds to Windows' default 96dpi
   (dots-per-cm :initform 38 :reader dots-per-cm)
   (target-resolution :initarg :target-resolution :initform (px-size 1920 1080) :accessor target-resolution)
   (resolution-scale :initform 1.0 :accessor resolution-scale)
   (base-scale :initarg :base-scale :initform 1.0 :accessor base-scale))
  (:default-initargs
   :layout-tree (make-instance 'layout-tree)
   :focus-tree (make-instance 'focus-tree)))

(defmethod focused ((ui ui))
  (focused (focus-tree ui)))

(defmethod handle ((event direct-event) (all (eql T)) (ui ui))
  (handle event (focus-tree ui) ui))

(defmethod handle ((event pointer-event) (all (eql T)) (ui ui))
  (unless (handle event (focus-tree ui) ui)
    (handle event (layout-tree ui) ui)))

(defmethod render ((ui ui) (thing (eql T)))
  (render ui (layout-tree ui)))

(defmethod maybe-render ((ui ui) (thing (eql T)))
  (maybe-render ui (layout-tree ui)))

(defmethod activate ((ui ui))
  (mark-for-render (root (layout-tree ui))))

(defmethod bounds ((ui ui))
  (bounds (root (layout-tree ui))))

(defmethod suggest-bounds (extent (ui ui))
  ;; TODO: decouple resolution setting from root layout
  ;;       since the resolution could be applicable to
  ;;       the screen, rather than the current ui window
  (let* ((target (target-resolution ui))
         (wr (/ (pxw extent) (pxw target)))
         (hr (/ (pxh extent) (pxh target))))
    (setf (resolution-scale ui) (min wr hr)))
  (suggest-bounds extent (layout-tree ui)))

(defmethod register ((source ui) (target ui))
  (register (layout-tree source) target))

;; TODO: Window sub-protocol
