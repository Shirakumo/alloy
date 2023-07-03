(in-package #:org.shirakumo.alloy)

(defclass list-label (label)
  ())

;;; TODO: Reordering the elements via button press and via dragging.
;;;       Will require offsetting without disturbing parent layout.
;;;       Might be tricky, too, if within clip view and all that.
(defclass list-view (vertical-linear-layout focus-list component)
  ((allow-deletion :initarg :allow-deletion :initform NIL :accessor deletion-allowed-p)
   (allow-insertion :initarg :allow-insertion :initform NIL :accessor insertion-allowed-p)))

(make-observable '(setf index) '(index observable))

(defmethod initialize-instance :after ((component list-view) &key (allow NIL allow-p))
  (when allow-p
    (setf (deletion-allowed-p component) (find :delete allow))
    (setf (insertion-allowed-p component) (find :insert allow))
    (setf (reordering-allowed-p component) (find :reorder allow)))
  (let ((data (data component)))
    (refresh component)
    (on element (value data index)
      (declare (ignore value))
      (refresh (index-element index component)))
    (on push-element (value data index)
      (declare (ignore value))
      (enter (represent-for component (place-data (element data index))) component :index index))
    (on pop-element (data index)
      (remove (index-element index component) component))))

(defmethod refresh ((component list-view))
  (clear component)
  (loop with data = (data component)
        for i from 0 below (count data)
        do (let ((i i))
             (enter (represent-for component (place-data (element data i))) component))))

(defmethod represent-for ((component list-view) data &rest initargs)
  (apply #'represent-with 'list-label data initargs))

(defgeneric make-new-element (list-view))

(defmethod handle ((event key-down) (component list-view))
  (case (key event)
    (:delete
     (if (deletion-allowed-p component)
         (pop-element (data component) (index component))
         (call-next-method)))
    (:insert
     (if (insertion-allowed-p component)
         (push-element (make-new-element component) (data component) (index component))
         (call-next-method)))
    (T (call-next-method))))

(defmethod component-class-for-object ((sequence vector))
  (find-class 'list-view))

;;; Note: Not doing COMPONENT-CLASS-FOR-OBJECT for CONS or NULL,
;;;       as that would much too easily lead to constructing
;;;       unexpected components.
