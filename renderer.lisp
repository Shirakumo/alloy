(in-package #:org.shirakumo.alloy)

(global-vars:define-global-var +renderer+ NIL)

(defgeneric allocate (renderer))
(defgeneric deallocate (renderer))
(defgeneric allocated-p (renderer))
(defgeneric register (renderable renderer))
(defgeneric deregister (renderable renderer))

(defgeneric render-needed-p (renderable))
(defgeneric mark-for-render (renderable))
(defgeneric render (renderer renderable))
(defgeneric maybe-render (renderer renderable))
(defgeneric constrain-visibility (extent renderer))
(defgeneric reset-visibility (renderer))
(defgeneric extent-visible-p (extent renderer))
(defgeneric translate (renderer point))

;; TODO: Replace 'call with constrained visibility' with 'constrain-visibility' and 'clear-visibility'
;;       and then mandate that visibility constraints are only applicable within a RENDER call.
;;       thus combining the current dual-use of c-w-c-v and w-p-m.

(defclass renderer ()
  ((allocated-p :initform NIL :reader allocated-p)
   (visible-bounds :initform (%extent (%px most-negative-single-float)
                                      (%px most-negative-single-float)
                                      (%px float-features:single-float-positive-infinity)
                                      (%px float-features:single-float-positive-infinity)) :accessor visible-bounds)))

(defmethod make-instance :after ((renderer renderer) &key)
  (setf +renderer+ renderer))

(defmethod constrain-visibility ((extent extent) (renderer renderer))
  (setf (visible-bounds renderer) (extent-intersection extent (visible-bounds renderer))))

(defmethod constrain-visibility ((size size) (renderer renderer))
  (constrain-visibility (extent 0 0 (size-w size) (size-h size)) renderer))

(defmethod reset-visibility ((renderer renderer))
  (setf (extent-x (visible-bounds renderer)) (px most-negative-single-float))
  (setf (extent-y (visible-bounds renderer)) (px most-negative-single-float))
  (setf (extent-w (visible-bounds renderer)) (px float-features:single-float-positive-infinity))
  (setf (extent-h (visible-bounds renderer)) (px float-features:single-float-positive-infinity)))

(defmethod extent-visible-p ((extent extent) (renderer renderer))
  (overlapping-p extent (visible-bounds renderer)))

(defmethod allocate :after ((renderer renderer))
  (setf (slot-value renderer 'allocated-p) T))

(defmethod deallocate :after ((renderer renderer))
  (setf (slot-value renderer 'allocated-p) NIL))

(defclass renderable ()
  ((render-needed-p :initform T :reader render-needed-p)))

(defmethod mark-for-render ((renderable renderable))
  (setf (slot-value renderable 'render-needed-p) T))

(defmethod render :after ((renderer renderer) (renderable renderable))
  (setf (slot-value renderable 'render-needed-p) NIL))

(defmethod maybe-render :around ((renderer renderer) (renderable renderable))
  (if (render-needed-p renderable)
      (render renderer renderable)
      (call-next-method)))

(defmethod register ((renderable renderable) (default (eql T)))
  (register renderable +renderer+))

(defmethod deregister ((renderable renderable) (default (eql T)))
  (deregister renderable +renderer+))

(defmethod render ((default (eql T)) (renderable renderable))
  (render +renderer+ renderable))

(defmethod maybe-render ((default (eql T)) (renderable renderable))
  (render +renderer+ renderable))

(defmethod constrain-visibility (extent (default (eql T)))
  (constrain-visibility extent +renderer+))

(defmethod reset-visibility ((default (eql T)))
  (reset-visibility +renderer+))

(defmethod extent-visible-p (extent (default (eql T)))
  (extent-visible-p extent +renderer+))

(defmethod translate ((default (eql T)) point)
  (translate +renderer+ point))
