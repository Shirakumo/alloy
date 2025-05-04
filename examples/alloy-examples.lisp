(in-package #:org.shirakumo.alloy.examples)

(defmethod title ((place alloy:value-data))
  (let ((value (alloy:value place)))
    (if value
        (symbol-name value)
        "")))

(defmethod description ((place alloy:value-data))
  (let ((value (alloy:value place)))
    (if value
        (or (documentation value 'function)
            "")
        "<no example selected>")))

(defmethod (setf title) (value (place alloy:value-data)))

(defmethod (setf description) (value (place alloy:value-data)))

(defun %meta-example (screen)
  (let* ((window (windowing:make-window screen :preferred-size (alloy:px-size 1200 800)))
         (focus (make-instance 'alloy:focus-list :focus-parent window))
         (layout (make-instance 'alloy:grid-layout :row-sizes '(T)
                                                   :col-sizes '(300 20 T)))
         (data (make-instance 'alloy:value-data :value NIL)))
    (setf (windowing:title window) "Alloy Examples")
    (alloy:enter layout window)
    (let ((list (make-instance 'alloy:vertical-linear-layout)))
      (alloy:enter list layout :row 0 :col 0)
      (dolist (example (sort (list-examples) #'string< :key #'symbol-name ; #'title
                             ))
        (make-instance 'alloy:button* :value (symbol-name example) ; (title example)
                                      :layout-parent list
                                      :focus-parent focus
                                      :on-activate (lambda ()
                                                     (setf (alloy:value data) example)
                                                     (alloy:refresh layout)))))
    (let ((details (make-instance 'alloy:grid-layout :col-sizes '(T)
                                                     :row-sizes '(50 T 50)))
          (launch (make-instance 'alloy:button* :value "Launch"
                                                :focus-parent focus
                                                :on-activate (lambda ()
                                                               ;; TODO(jmoringe): this could break for some backends, i think
                                                               (let ((value (alloy:value data)))
                                                                (when value
                                                                  (throw 'example-selected
                                                                    value)))))))
      (alloy:enter details layout :row 0 :col 2)
      (alloy:represent (title data) 'alloy:label :layout-parent details
                                                 :style `((:label :size ,(alloy:un 30))))
      (alloy:represent (description data) 'alloy:label :layout-parent details
                                                       :style `((:label :valign :top :wrap T)))
      (alloy:enter launch details))))

(defun meta-example ()
  (loop for example = (catch 'example-selected
                        (if (boundp '*screen*)
                            (%meta-example *screen*)
                            (glfw:with-screen (*screen* 'screen :base-scale 2.0)
                              (%meta-example *screen*))))
        while example
        do (funcall example)))

(defun launch (&optional (example 'meta-example) &rest args)
  (apply (find-symbol (string example) #.*package*) args))
