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
  (let ((solver (solver layout)))
    (format *trace-output* "~D variables, ~D constraints, ~D expressions~%"
            (hash-table-count (slot-value solver 'cass::variables))
            (hash-table-count (slot-value solver 'cass::constraints))
            (hash-table-count (slot-value solver 'cass::expressions))))
  (alloy:do-elements (element layout)
    (suggest-size-and-install-bounds layout element)))

(defun suggest-size-and-install-bounds (layout element)
  (alloy:with-unit-parent layout
    (with-vars (x y w h layout) element
      (let* ((preferred-size (alloy:suggest-size
                              (alloy:px-size (alloy:to-px (alloy:un (cass:value w)))
                                             (alloy:to-px (alloy:un (cass:value h))))
                              element)))
        (format *trace-output* "Preferred size~%  ~A~%  variables     ~A ~A~%  suggest-size  ~A ~A~%  =>            ~A~%"
                element
                (cass:value w) (cass:value h)
                (alloy:to-px (alloy:un (cass:value w))) (alloy:to-px (alloy:un (cass:value h)))
                preferred-size)
        (cass:suggest w (alloy:to-un (alloy:px (alloy:w preferred-size))))
        (cass:suggest h (alloy:to-un (alloy:px (alloy:h preferred-size))))
        (cass:update-variables (solver layout))
        (format *trace-output* "  new variables ~A ~A~%"
                (cass:value w) (cass:value h))
        (install-bounds layout element)))))

(defun find-element-constraints (element layout)
  (let ((all-constraints (constraints layout)))
    (or (gethash element all-constraints)
        (setf (gethash element all-constraints) (cons '() '())))))

(defun install-user-constraints (constraints element layout)
  (let ((solver (solver layout))
        (element-constraints (find-element-constraints element layout)))
    (dolist (expression constraints layout)
      (multiple-value-bind (expressions strength) (transform-expression expression)
        (dolist (expression expressions)
          (let ((constraint (cass:constrain-with solver (rewrite-expression expression element layout)
                                                 :strength (or strength :strong))))
            (format *trace-output* "Adding constraint ~A~%" constraint)
            (push constraint (car element-constraints))))))))

(defun install-minimum-size-constraints (element layout)
  (let ((solver (solver layout))
        (element-constraints (find-element-constraints element layout)))
    (flet ((add-constraint (expression)
             (let ((constraint (cass:constrain-with solver expression
                                                    :strength :strong)))
               (push constraint (cdr element-constraints)))))
      (alloy:with-unit-parent layout
        (with-vars (x y w h layout) element
          (declare (ignore x y))
          (let* ((min-size (alloy:suggest-size (alloy:size) element))
                 (min-width (alloy:to-un (alloy:px (alloy:w min-size))))
                 (min-height (alloy:to-un (alloy:px (alloy:h min-size)))))
            (add-constraint `(>= ,w ,min-width))
            (add-constraint `(>= ,h ,min-height))))))))

(defmethod alloy:enter ((element alloy:layout-element) (layout layout) &key constraints)
  (call-next-method)
  (let ((solver (solver layout)))
    (setf (gethash element (variables layout))
          (list (cass:make-variable solver :name (format NIL "X ~a" element))
                (cass:make-variable solver :name (format NIL "Y ~a" element))
                (cass:make-variable solver :name (format NIL "W ~a" element) :strength :medium)
                (cass:make-variable solver :name (format NIL "H ~a" element) :strength :medium)))
    (when constraints
      (install-user-constraints constraints element layout))
    (when (alloy:layout-tree layout)
      (install-minimum-size-constraints element layout)
      (update-and-install-all-bounds layout))))

(defmethod alloy:leave :after ((element alloy:layout-element) (layout layout))
  (let ((element-constraints (find-element-constraints element layout)))
    (mapc #'cass:delete-constraint (car element-constraints))
    (mapc #'cass:delete-constraint (cdr element-constraints)))
  (remhash element (constraints layout))
  (let ((variables (variables layout)))
    (mapc #'cass:delete-variable (gethash element variables))
    (remhash element variables))
  ;; TODO update
  )

(defmethod alloy:update ((element alloy:layout-element) (layout layout) &key constraints clear)
  (when clear
    (let ((element-constraints (find-element-constraints element layout)))
      (mapc (lambda (c)
              (format *trace-output* "Deleting constraint ~A~%" c)
              (cass:delete-constraint c))
            (car element-constraints))
      (setf (car element-constraints) '())))
  (when constraints
    (install-user-constraints constraints element layout))
  ;; An :AFTER method calls NOTICE-SIZE which will trigger all the updates.
  ; (update-and-install-all-bounds layout)
  )

(defmethod alloy:notice-size ((element alloy:layout-element) (layout layout))
  ;; Update constraints for minimal size.
  (let ((element-constraints (find-element-constraints element layout)))
    (mapc #'cass:delete-constraint (cdr element-constraints))
    (setf (cdr element-constraints) '()))
  (install-minimum-size-constraints element layout)
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
      (alloy:size (alloy:un (cass:value w)) (alloy:un (cass:value h))))))
