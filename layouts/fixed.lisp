(in-package #:org.shirakumo.alloy)

(defclass fixed-layout (layout vector-container)
  ())

(defmethod notice-size ((element layout-element) (layout fixed-layout)))

(defmethod extent-visible-p ((layout fixed-layout) (renderer renderer))
  T)

(defmethod suggest-size (size (layout fixed-layout))
  size)

(defmethod enter ((element layout-element) (layout fixed-layout) &key x y w h extent)
  (call-next-method)
  (let ((e (bounds element)))
    (when (layout-tree layout)
      (with-unit-parent layout
        (setf (bounds element)
              (px-extent (or x (when extent (extent-x extent)) (extent-x e))
                         (or y (when extent (extent-y extent)) (extent-y e))
                         (or w (when extent (extent-w extent)) (extent-w e))
                         (or h (when extent (extent-h extent)) (extent-h e))))))
    element))

(defmethod update ((element layout-element) (layout fixed-layout) &key x y w h extent)
  (call-next-method)
  (let ((e (bounds element)))
    (with-unit-parent layout
      (setf (bounds element)
            (px-extent (or x (when extent (extent-x extent)) (extent-x e))
                       (or y (when extent (extent-y extent)) (extent-y e))
                       (or w (when extent (extent-w extent)) (extent-w e))
                       (or h (when extent (extent-h extent)) (extent-h e)))))
    element))

(defmethod ensure-visible :before ((element layout-element) (layout fixed-layout))
  ;; Find parent
  (loop until (or (eq layout (layout-parent element))
                  (eq element (layout-parent element)))
        do (setf element (layout-parent element)))
  (when (eq layout (layout-parent element))
    ;; Shuffle to ensure element is last, and thus drawn on top.
    (rotatef (aref (elements layout) (1- (length (elements layout))))
             (aref (elements layout) (position element (elements layout))))))

(defmethod refit ((layout fixed-layout)))
