#|
 This file is a part of Alloy
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.alloy)

(defclass combo-item (button direct-value-component)
  ())

(defclass combo-layout (vertical-linear-layout)
  ())

(defmethod handle ((event scroll) (layout combo-layout))
  (let ((extent (bounds layout)))
    ;; FIXME: constrain the scrolling to match the parent combo box.
    (setf (bounds layout)
          (px-extent (pxx extent)
                     (+ (pxy extent) (* -20 (dy event)))
                     (pxw extent)
                     (pxh extent)))))

(defclass combo (value-component focus-list)
  ((state :initform NIL :accessor state)
   (combo-list :initform (make-instance 'combo-layout :layout-parent NIL :cell-margins (margins)) :reader combo-list)))

(defgeneric combo-item (item combo))
(defgeneric value-set (data))
(define-observable (setf value-set) (set observable))

(defmethod value-changed :after ((combo combo))
  ;; FIXME: This is bad
  (do-elements (element combo)
    (when (eql (value combo) (value element))
      (return (setf (focused combo) element)))))

(defmethod initialize-instance :after ((combo combo) &key)
  (update-combo-items combo (value-set combo))
  (on (setf value-set) (set (data combo))
    (update-combo-items combo set)))

(defmethod set-layout-tree :before (value (combo combo))
  (set-layout-tree value (combo-list combo)))

(defmethod text ((combo combo))
  (if (focused combo)
      (text (focused combo))
      ""))

(defmethod activate :after ((combo combo))
  (setf (state combo) :selecting))

(defmethod handle ((event key-up) (combo combo))
  (case (key event)
    (:up
     (focus-prev combo))
    (:down
     (focus-next combo))
    (:enter
     (setf (value combo) (value (focused combo))))
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
     (render renderer (combo-list combo)))))

(defmethod (setf bounds) :after (bounds (combo combo))
  (let ((ideal (suggest-bounds bounds (combo-list combo))))
    (setf (bounds (combo-list combo))
          (px-extent (pxx bounds)
                     (+ (- (pxy bounds) (pxh ideal)) (pxh bounds))
                     (pxw ideal)
                     (pxh ideal)))))

(defclass combo-set (combo)
  ((value-set :initform (arg! :value-set) :initarg :value-set :accessor value-set)))

(defmethod (setf value-set) :after (set (combo combo-set))
  (update-combo-items combo set))
