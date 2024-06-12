(in-package #:org.shirakumo.alloy.layouts.constraint)

(defun make-variables (element solver &key strength)
  (list (cass:make-variable solver :name (format NIL "X ~a" element) :strength strength)
        (cass:make-variable solver :name (format NIL "Y ~a" element) :strength strength)
        (cass:make-variable solver :name (format NIL "W ~a" element) :strength strength)
        (cass:make-variable solver :name (format NIL "H ~a" element) :strength strength)))

(defclass layout (alloy:layout alloy:layout-element alloy:vector-container)
  ((solver :initform (cass:make-solver) :reader solver)
   (variables :initform (make-hash-table :test 'eq) :reader variables)
   (constraints :initform (make-hash-table :test 'eq) :reader constraints)))

(defmethod initialize-instance ((layout layout) &key)
  (call-next-method)
  (setf (gethash layout (variables layout))
        (make-variables layout (solver layout) :strength :strong)))

(defun suggest-size (layout element size)
  (with-vars (x y w h layout) element
    (declare (ignore x y))
    (cass:suggest w (alloy:to-un (alloy:size-w size)))
    (cass:suggest h (alloy:to-un (alloy:size-h size)))
    (cass:update-variables (solver layout))))

(defun element-var (layout element var)
  (with-vars (x y w h layout) element
    (ecase var
      (:x x)
      (:y y)
      (:w w)
      (:h h))))

(defun install-bounds (layout element)
  (with-vars (x y w h layout) element
    (setf (alloy:bounds element)
          (alloy:px-extent (alloy:un (cass:value x)) (alloy:un (cass:value y))
                           (alloy:un (cass:value w)) (alloy:un (cass:value h))))))

(defun update-and-install-all-bounds (layout)
  (cass:update-variables (solver layout))
  (alloy:do-elements (element layout)
    (suggest-size-and-install-bounds layout element)))

(defun suggest-size-and-install-bounds (layout element)
  (alloy:with-unit-parent layout
    (with-vars (x y w h layout) element
      (let* ((preferred-size (alloy:suggest-size
                              (alloy:size (alloy:un (cass:value w))
                                          (alloy:un (cass:value h)))
                              element)))
        (format *trace-output* "Preferred size~%  ~A~%=>~A~%" element preferred-size)
        (cass:suggest w (alloy:to-un (alloy:w preferred-size)))
        (cass:suggest h (alloy:to-un (alloy:h preferred-size)))
        (cass:update-variables (solver layout))
        (install-bounds layout element)))))

#+unused (defun suggest (layout element var size)
  (alloy:with-unit-parent layout
    (cass:suggest (element-var layout element var) (alloy:to-un size) :make-suggestable)
    (cass:update-variables (solver layout))
    (alloy:do-elements (element layout)
      (install-bounds layout element))))

#+no (defun constrain (layout element var value &key (strength :required))
  (alloy:with-unit-parent layout
    (prog1 (cass:constrain-with (solver layout) `(= ,(element-var layout element var) ,(alloy:to-un value)) :strength strength)
      (update-and-install-all-bounds layout))))

(defmethod alloy:enter ((element alloy:layout-element) (layout layout) &key constraints)
  (call-next-method)
  (let ((solver (solver layout)))
    (setf (gethash element (variables layout))
          (list (cass:make-variable solver :name (format NIL "X ~a" element))
                (cass:make-variable solver :name (format NIL "Y ~a" element))
                (cass:make-variable solver :name (format NIL "W ~a" element) :strength :medium)
                (cass:make-variable solver :name (format NIL "H ~a" element) :strength :medium))))
  (when constraints
    (apply-constraints constraints element layout))
  (when (alloy:layout-tree layout)
    (alloy:with-unit-parent layout
      (with-vars (x y w h layout) element
        (declare (ignore x y))
        (let ((min-size (alloy:suggest-size (alloy:size) element)))
          (cass:constrain-with (solver layout) `(>= ,w ,(alloy:to-un (alloy:w min-size))) :strength :strong)
          (cass:constrain-with (solver layout) `(>= ,h ,(alloy:to-un (alloy:h min-size))) :strength :strong))
        ;; (suggest-size layout layout (alloy:bounds layout))
        ))
    (update-and-install-all-bounds layout)))

(defmethod alloy:leave :after ((element alloy:layout-element) (layout layout))
  (dolist (constraint (gethash element (constraints layout)))
    (cass:delete-constraint constraint)) ; TODO update
  (remhash element (variables layout)))

(defmethod alloy:update ((element alloy:layout-element) (layout layout) &key constraints clear)
  (when clear
    (dolist (constraint (gethash element (constraints layout)))
      (cass:delete-constraint constraint)))
  (apply-constraints constraints element layout))

(defmethod alloy:notice-size ((element alloy:layout-element) (layout layout))
  ;; (update-and-install-all-bounds layout)
  (alloy:refit layout))

(defmethod (setf alloy:bounds) :after (extent (layout layout))
  (alloy:refit layout))

(defmethod alloy:refit ((layout layout))
  (let ((bounds (alloy:bounds layout)))
    (alloy:with-unit-parent layout
      (let ((width (alloy:to-un (alloy:size-w bounds)))
            (height (alloy:to-un (alloy:size-h bounds))))
        (with-vars (x y w h layout) layout
          (declare (ignore x y))
          (cass:suggest w width)
          (cass:suggest h height)))))
  (update-and-install-all-bounds layout)
  #+no (alloy:with-unit-parent layout

         #+no (suggest-size layout layout (alloy:bounds layout))
         #+no (alloy:do-elements (element layout)
                (install-bounds layout element))))

(defmethod alloy:suggest-size (size (layout layout))
  (alloy:with-unit-parent layout
    ;; FIXME: LAYOUT
    (suggest-size layout layout size)
    (with-vars (x y w h layout) layout
      (declare (ignore x y))
      (alloy:size (cass:value w) (cass:value h)))))

(defun apply-constraints (constraints element layout)
  (dolist (expression constraints layout)
    (multiple-value-bind (expressions strength) (transform-expression expression)
      (dolist (expression expressions)
        (push (cass:constrain-with (solver layout) (rewrite-expression expression element layout) :strength (or strength :strong))
              (gethash element (constraints layout)))))))
