#|
 This file is a part of Alloy
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.alloy)

(defclass combo-item (button direct-value-component)
  ())

(defclass combo-layout (vertical-linear-layout)
  ((parent :initarg :parent :accessor parent)))

(defmethod handle ((event scroll) (layout combo-layout))
  (let ((extent (bounds layout))
        (pextent (bounds (parent layout))))
    (setf (bounds layout)
          (px-extent 0
                     ;; FIXME: still not correct.
                     (max (+ (pxy extent) (* -20 (dy event)))
                          (- (pxh pextent) (pxh extent)))
                     (pxw extent)
                     (pxh extent)))))

(defclass combo (value-component focus-list)
  ((state :initform NIL :accessor state)
   (combo-list :reader combo-list)))

(defmethod initialize-instance ((combo combo) &key)
  (call-next-method)
  (setf (slot-value combo 'combo-list) (make-instance 'combo-layout :cell-margins (margins) :parent combo))
  (setf (slot-value (slot-value combo 'combo-list) 'layout-parent) combo))

(defgeneric combo-item (item combo))
(defgeneric value-set (data))
(define-observable (setf value-set) (set observable))

(defmethod notice-size (thing (combo combo)))

(defmethod value-changed :after ((combo combo))
  ;; FIXME: This is bad
  (do-elements (element combo)
    (when (eql (value combo) (value element))
      (return (setf (focused combo) element)))))

(defmethod initialize-instance :after ((combo combo) &key)
  (update-combo-items combo (value-set combo))
  (on value-set (set (data combo))
    (update-combo-items combo set)))

(defmethod set-layout-tree :before (value (combo combo))
  (set-layout-tree value (combo-list combo)))

(defmethod (setf index) :after (index (combo combo))
  (let ((ib (bounds (focused combo)))
        (lb (bounds (combo-list combo))))
    (setf (bounds (combo-list combo))
          (px-extent (pxx lb)
                     (- 0 (- (pxy ib) (pxy lb)))
                     (pxw lb)
                     (pxh lb)))))

(defmethod text ((combo combo))
  (if (focused combo)
      (text (focused combo))
      (princ-to-string (value combo))))

(defmethod activate :after ((combo combo))
  (setf (state combo) :selecting)
  (handle (make-instance 'scroll :dx 0.0 :dy 0.0 :location (point 0 0)) (combo-list combo)))

(defmethod handle ((event key-down) (combo combo))
  (case (key event)
    (:up
     (focus-prev combo))
    (:down
     (focus-next combo))
    (:enter
     (when (focused combo)
       (setf (value combo) (value (focused combo)))))
    (T (call-next-method))))

(defmethod handle :around ((event pointer-event) (combo combo))
  (case (state combo)
    (:selecting
     (unless (handle event (combo-list combo))
       (when (typep event 'pointer-up)
         (exit combo)))
     T)
    (T
     (call-next-method))))

(defmethod handle ((event scroll) (combo combo))
  (cond ((< 0 (dy event))
         (focus-prev combo))
        ((< (dy event) 0)
         (focus-next combo)))
  (setf (value combo) (value (focused combo))))

(defmethod (setf focus) :after (focus (combo combo))
  (when (null focus)
    (setf (state combo) NIL)))

(defmethod (setf value) :after (value (combo combo))
  (exit combo))

(defmethod combo-item (item (combo combo))
  (make-instance 'combo-item :value item))

(defmethod update-combo-items ((combo combo) items)
  (let ((list (combo-list combo)))
    ;; TODO: It may be possible to optimise this to only insert and remove
    ;;       items as necessary, but ensuring the order is as specified in
    ;;       the items list seems difficult to do without sacrificing efficiency.
    (clear list)
    (clear combo)
    (flet ((add (el)
             (let ((item (combo-item el combo)))
               (enter item list)
               (enter item combo)
               (on activate (item)
                 (setf (value combo) (value item))))))
      (etypecase items
        (list (loop for item in items do (add item)))
        (vector (loop for item across items do (add item)))))))

(defmethod register :after ((combo combo) (renderer renderer))
  (register (combo-list combo) renderer))

(defmethod render :after ((renderer renderer) (combo combo))
  (case (state combo)
    (:selecting
     (reset-visibility renderer)
     (render renderer (combo-list combo)))))

(defmethod (setf bounds) :after (bounds (combo combo))
  (let ((ideal (suggest-size bounds (combo-list combo))))
    (setf (bounds (combo-list combo))
          (px-extent 0
                     (- (pxh bounds) (pxh ideal))
                     (pxw ideal)
                     (pxh ideal)))))

(defclass combo-set (combo)
  ((value-set :initform (arg! :value-set) :initarg :value-set :accessor value-set)))

(defmethod (setf value-set) :after (set (combo combo-set))
  (update-combo-items combo set))
