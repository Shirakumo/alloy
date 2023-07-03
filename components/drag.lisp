(in-package #:org.shirakumo.alloy)

(defclass draggable (component)
  ((initial-pos :initform NIL :accessor initial-pos)))

(define-observable drag-to (location observable))

(defmethod drag-to (location (draggable draggable))
  )

(defmethod handle ((event pointer-down) (draggable draggable))
  (setf (initial-pos draggable) (location event))
  (activate draggable))

(defmethod handle ((event pointer-up) (draggable draggable))
  (setf (initial-pos draggable) NIL)
  (exit draggable))

(defmethod handle ((event pointer-move) (draggable draggable))
  (when (initial-pos draggable)
    (drag-to (location event) draggable)))

(defmethod handle ((event button-down) (draggable draggable))
  (case (button event)
    (:a (setf (initial-pos draggable) (px-point 0 0)))
    (T (call-next-method))))

(defmethod handle ((event button-up) (draggable draggable))
  (case (button event)
    (:a (setf (initial-pos draggable) NIL))
    (T (call-next-method))))

(defclass resizer (draggable)
  ((side :initarg :side :accessor side)
   (initial-bounds :initform NIL :accessor initial-bounds)))

(defmethod (setf initial-pos) :after (value (resizer resizer))
  (setf (initial-bounds resizer) (if value
                                     (copy-extent (bounds (data resizer)))
                                     NIL)))

(defmethod drag-to (location (resizer resizer))
  (let* ((dx (- (pxx location) (pxx (initial-pos resizer))))
         (dy (- (pxy location) (pxy (initial-pos resizer))))
         (target (data resizer))
         (ib (initial-bounds resizer)))
    (ecase (side resizer)
      (:north
       (setf (bounds target) (px-extent (x ib) (y ib) (w ib) (+ (pxh ib) dy))))
      (:east
       (setf (bounds target) (px-extent (x ib) (y ib) (+ (pxw ib) dx) (h ib))))
      (:south
       (setf (bounds target) (px-extent (x ib) (+ (pxy ib) dy) (w ib) (- (pxh ib) dy))))
      (:west
       (setf (bounds target) (px-extent (+ (pxx ib) dx) (y ib) (- (pxw ib) dx) (h ib)))))
    (notice-size target (layout-parent target))))

(defmethod handle :before ((event pointer-move) (resizer resizer))
  (if (or (contained-p (location event) resizer)
          (initial-pos resizer))
      (setf (cursor (ui resizer)) (ecase (side resizer)
                                    ((:north :south) :ns-resize)
                                    ((:east :west) :ew-resize)))
      (setf (cursor (ui resizer)) NIL)))

(defmethod suggest-size (size (resizer resizer))
  (size (un 5) (un 5)))
