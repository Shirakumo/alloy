(in-package #:org.shirakumo.alloy)

(defclass value-component (component)
  ((value-function :initarg :value-function :initform 'value :reader value-function)))

(defmethod initialize-instance :after ((component value-component) &key)
  (observe (value-function component) (data component)
           (lambda (value observable)
             (declare (ignore value observable))
             (value-changed component))
           component))

(defmethod print-object ((element value-component) stream)
  (print-unreadable-object (element stream :type T :identity T)
    (handler-case
        (let ((*print-pretty* t)
              (*print-circle* t)
              (*print-length* 3)
              (*print-level* 3)
              (*print-lines* 3))
          (princ (value element) stream))
      (error ()
        (write-string "<error printing component value>" stream)))
    (format stream " ~a~@[ focus ~a~]" (bounds element) (focus element))))

(defmethod value-changed ((component value-component))
  (mark-for-render component))

(defmethod value ((component value-component))
  (access (data component) (value-function component)))

(defmethod (setf value) (new-value (component value-component))
  (setf (access (data component) (value-function component)) new-value))

(defmethod refresh ((component value-component))
  (setf (value component) (value component)))

(defmethod text ((component value-component))
  (princ-to-string (value component)))

(defclass direct-value-component (value-component)
  ((value :initarg :value :accessor value)
   (data :initform ())))

(defmethod initialize-instance ((component direct-value-component) &key data)
  (when data (error "DATA is not allowed for a ~s" (type-of component)))
  (call-next-method)
  (setf (slot-value component 'data) component))

(defclass progress (value-component)
  ((maximum :initarg :maximum :initform 100 :accessor maximum)))

(defclass label (value-component)
  ((wrap :initarg :wrap :initform NIL :accessor wrap)))

(defmethod component-class-for-object ((string string))
  (find-class 'label))

(defclass label* (label direct-value-component) ())

(defmethod enter ((string string) (layout layout) &rest args)
  (apply #'enter (make-instance 'label* :value string) layout args))

(defclass icon (value-component)
  ())

(defmethod register ((icon icon) (renderer renderer))
  (register (value icon) renderer))

(defclass icon* (icon direct-value-component) ())
