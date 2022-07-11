#|
 This file is a part of Alloy
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.alloy)

(defgeneric handle (event handler))

(defclass event ()
  ())

(defun decline ()
  (invoke-restart 'decline))

(defmethod handle :around ((event event) handler)
  (restart-case
      (prog1 T
        (call-next-method))
    (decline ()
      :report (lambda (s) (format s "Decline handling ~a" event))
      NIL)))

(defclass input-event (event)
  ())

(defclass pointer-event (input-event)
  ((location :initarg :location :initform (arg! :location) :reader location)))

(defclass pointer-move (pointer-event)
  ((old-location :initarg :old-location :initform (arg! :old-location) :reader old-location)))

(defclass pointer-down (pointer-event)
  ((kind :initarg :kind :initform :left :reader kind)))

(defclass pointer-up (pointer-event)
  ((kind :initarg :kind :initform :left :reader kind)))

(defclass scroll (pointer-event)
  ((dx :initarg :dx :initform (arg! :dx) :reader dx)
   (dy :initarg :dy :initform (arg! :dy) :reader dy)))

(defclass direct-event (event)
  ())

(defclass copy-event (direct-event)
  ())

(defclass paste-event (direct-event)
  ((content :initarg :content :initform (arg! :content) :reader content)))

(defclass cut-event (direct-event)
  ())

(defclass drop-event (direct-event)
  ((paths :initarg :paths :initform (arg! :paths) :reader paths)))
;; FIXME: drop-event should be a pointer event and be handled on the thing it's being dropped on.

(defclass text-event (direct-event)
  ((text :initarg :text :initform (arg! :text) :reader text)))

(defclass key-event (input-event direct-event)
  ((key :initarg :key :initform (arg! :key) :reader key)
   (code :initarg :code :initform (arg! :code) :reader code)
   (modifiers :initarg :modifiers :initform () :reader modifiers)))

(defclass key-down (key-event)
  ())

(defclass key-up (key-event)
  ())

(defclass button-event (input-event direct-event)
  ((button :initarg :button :initform (arg! :button) :reader button)
   (device :initarg :device :initform (arg! :device) :reader device)))

(defclass button-down (button-event)
  ())

(defclass button-up (button-event)
  ())

;; TODO: standard translation of button/key/pointer etc events to focus events.

(defclass focus-event (direct-event)
  ())

(defclass focus-next (focus-event)
  ())

(defclass focus-prev (focus-event)
  ())

(defclass focus-up (focus-event)
  ())

(defclass focus-down (focus-event)
  ())

(defclass focus-left (focus-event)
  ())

(defclass focus-right (focus-event)
  ())

(defclass activate (focus-event)
  ())

(defclass exit (focus-event)
  ())
