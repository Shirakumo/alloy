(in-package #:org.shirakumo.alloy)

(defclass component (observable layout-element focus-element renderable)
  ((data :initarg :data :initform (arg! :data) :accessor data)
   (tooltip :initarg :tooltip :initform NIL :accessor tooltip)
   (%tooltip :initform NIL :accessor %tooltip)))

(defmethod print-object ((element component) stream)
  (print-unreadable-object (element stream :type T :identity T)
    (format stream " ~a~@[ focus ~a~]" (bounds element) (focus element))))

(defmethod handle ((event pointer-down) (component component))
  (unless (and (slot-boundp component 'focus-parent)
               (contained-p (location event) component))
    (call-next-method)))

(defmethod handle ((event pointer-up) (component component))
  (if (and (slot-boundp component 'focus-parent)
           (contained-p (location event) component)
           (eql :left (kind event)))
      (activate component)
      (call-next-method)))

(defmethod set-layout-tree :after ((none null) (component component))
  (when (%tooltip component)
    (leave (%tooltip component) T)
    (setf (%tooltip component) NIL)))

(defmethod handle ((event pointer-move) (component component))
  (cond ((and (slot-boundp component 'focus-parent)
              (contained-p (location event) component))
         (when (eql NIL (focus component))
           (setf (focus (focus-parent component)) :strong)
           (setf (focus component) :weak))
         (when (tooltip component)
           (unless (%tooltip component)
             (setf (%tooltip component) (make-instance 'tooltip :component component))
             (enter (%tooltip component) (ui component)))
           (let ((size (suggest-size (size 20 20) (%tooltip component)))
                 (margin 15))
             (update (%tooltip component) (ui component)
                     :x (max (min (+ (pxx (location event)) margin)
                                  (- (pxw (ui component)) (pxw size)))
                             0)
                     :y (max (min (- (pxy (location event)) (pxh size) margin)
                                  (- (pxh (ui component)) (pxh size)))
                             0)
                     :w (w size)
                     :h (h size)))))
        (T
         (when (%tooltip component)
           (leave (%tooltip component) T)
           (setf (%tooltip component) NIL))
         (call-next-method))))

(defmethod maybe-render ((renderer renderer) (component component)))

(defmethod (setf data) :after (value (component component))
  (mark-for-render component))

(defmethod (setf focus) :after (value (component component))
  (when (and value (layout-tree component))
    (ensure-visible component T))
  (mark-for-render component))

(defmethod (setf bounds) :after (value (component component))
  (mark-for-render component))

(make-observable '(setf focus) '(focus observable))
(make-observable '(setf bounds) '(bounds observable))
;(make-observable 'handle '(event observable))
(make-observable 'activate '(observable))
(make-observable 'exit '(observable))

(defgeneric component-class-for-object (data))
(defgeneric represent-with (component-type data &rest initargs))
(defgeneric represent-for (component data &rest initargs))

(defmacro represent (place type &rest initargs)
  `(represent-with ,type
                   ,(expand-place-data place)
                   ,@initargs))

(defmethod represent-with ((type (eql T)) (data data) &rest initargs)
  (let ((class (component-class-for-object (value data))))
    (apply #'represent-with class data initargs)))

(defmethod represent-with ((class class) data &rest initargs)
  (apply #'make-instance class :data data initargs))

(defmethod represent-with ((name symbol) data &rest initargs)
  (apply #'represent-with (find-class name) data initargs))

(defmethod leave ((component component) (parent (eql T)))
  (when (slot-boundp component 'layout-parent)
    (leave component (layout-parent component)))
  (when (slot-boundp component 'focus-parent)
    (leave component (focus-parent component))))
