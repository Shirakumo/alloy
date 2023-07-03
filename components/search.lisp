(in-package #:org.shirakumo.alloy)

(defclass searchable (vertical-linear-layout focus-list value-component)
  ())

(defmethod initialize-instance :after ((component searchable) &key data)
  (let ((results (make-instance 'list-view ))
        (query (make-instance 'input-line :data (make-instance 'value-data))))
    (enter query component)
    (enter (make-instance 'scroll-view :layout results :scroll :y) component)
    (on (setf value) (text query)
      )))

(defgeneric query (data query))
