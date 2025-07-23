(in-package #:org.shirakumo.alloy)

(defclass section-header (labelled-switch)
  ((layout-element :initform NIL :reader layout-element)
   (focus-element :initform NIL :reader focus-element)))

(defmethod value-changed :after ((header section-header))
  (let ((expanded-p (value header)))
    (flet ((frob (element parent slot)
             (let ((i (element-index header parent)))
               (if expanded-p
                   (unless (slot-boundp element slot) (enter element parent :index (1+ i)))
                   (when (slot-boundp element slot) (leave element parent))))))
      (when (layout-element header)
        ;; KLUDGE: this isn't so nice since the element can't animate at all.
        (frob (layout-element header) (layout-parent header) 'layout-parent))
      (when (focus-element header)
        (frob (focus-element header) (focus-parent header) 'focus-parent)))))

(defmethod enter ((element element) (header section-header) &key)
  (when (typep element 'focus-element)
    (setf (slot-value header 'focus-element) element))
  (when (typep element 'layout-element)
    (setf (slot-value header 'layout-element) element))
  (value-changed header)
  element)

(defmethod leave ((element element) (header section-header))
  (when (eq element (focus-element header))
    (leave element (focus-parent header))
    (setf (slot-value header 'focus-element) NIL))
  (when (eq element (layout-element header))
    (leave element (layout-parent header))
    (setf (slot-value header 'layout-element) NIL))
  (when (and (not (layout-element header))
             (not (focus-element header)))
    (leave header (focus-parent header))
    (leave header (layout-parent header)))
  element)

(defclass section-list (structure)
  ((sections :initform (make-array 0 :adjustable T :fill-pointer T) :accessor sections)))

(defmethod initialize-instance :after ((list section-list) &key focus-parent layout-parent)
  (finish-structure list
                    (make-instance 'vertical-linear-layout :layout-parent layout-parent
                                                           :cell-margins (margins))
                    (make-instance 'vertical-focus-list :focus-parent focus-parent)))

(defun ensure-section (list &key label index (expanded-p NIL expanded-p-p))
  (let* ((sections (sections list))
         (index (or index (length sections))))
    (when (<= (length sections) index)
      (let ((old (length sections)))
        (loop for i from old to index
              for header = (make-instance 'section-header :data (make-instance 'value-data :value T)
                                                          :text (format NIL "Section ~d" (1+ i)))
              do (vector-push-extend header sections)
                 (enter header (layout-element list))
                 (enter header (focus-element list)))))
    (let ((section (aref sections index)))
      (when expanded-p-p
        (setf (value section) expanded-p))
      (when label
        (setf (text section) label))
      section)))

(defmethod enter ((element element) (list section-list) &rest args &key label index expanded-p)
  (declare (ignore label index expanded-p))
  (enter element (apply #'ensure-section list args)))

(defmethod leave ((element element) (list section-list))
  (let ((section (or (find element (sections list) :test (lambda (x b) (or (eq x (layout-element b))
                                                                           (eq x (focus-element b)))))
                     (error 'element-not-contained :bad-element element :container list))))
    (leave element section)
    (unless (layout-tree section)
      (array-utils:vector-pop-element (sections list) section))
    element))

(defmethod enter ((elements cons) (list section-list) &rest args &key label index expanded-p)
  (declare (ignore label index expanded-p))
  (let ((section (apply #'ensure-section list args)))
    (enter (car elements) section)
    (enter (cdr elements) section)
    elements))

(defmethod leave ((elements cons) (list section-list))
  (leave (car elements) list)
  (leave (cdr elements) list)
  elements)

(defmethod enter ((structure structure) (list section-list) &rest args &key label index expanded-p)
  (declare (ignore label index expanded-p))
  (apply #'enter (cons (layout-element structure) (focus-element structure)) list args))
