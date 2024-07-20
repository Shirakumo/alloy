(in-package #:org.shirakumo.alloy.renderers.framebuffers)

(defclass renderer (simple:transformed-renderer)
  ())

(defclass raster-shape (simple:shape)
  ((sampler :initform (raster:ensure-sampler NIL) :accessor sampler)))

(defmethod initialize-instance :after ((shape raster-shape) &key pattern)
  (when pattern (setf (sampler shape) pattern)))

(defmethod (setf simple:pattern) :after (pattern (shape raster-shape))
  (setf (sampler shape) pattern))

(defmethod (setf sampler) ((pattern colored:color) (shape raster-shape))
  (setf (sampler shape) (raster:solid-color (round (* 255 (colored:r pattern)))
                                            (round (* 255 (colored:g pattern)))
                                            (round (* 255 (colored:b pattern)))
                                            (round (* 255 (colored:a pattern))))))

(defmethod (setf sampler) ((pattern simple:image) (shape raster-shape))
  (let ((size (simple:size pattern)))
    (setf (sampler shape) (raster:sampler (simple:data pattern)
                                          (alloy:pxw size)
                                          (alloy:pxh size)))))

(defmethod (setf sampler) ((pattern simple:image-pattern) (shape raster-shape))
  (let ((size (simple:size (simple:image pattern)))
        (scale (simple:scaling pattern))
        (offset (simple:offset pattern)))
    (setf (sampler shape) (raster:sampler (simple:data (simple:image pattern))
                                          (alloy:pxw size)
                                          (alloy:pxh size)
                                          :border (simple:mode pattern)
                                          :transform (convert-transform scale offset)))))

(defun convert-transform (scale offset)
  (raster:make-transform
   (alloy:pxw scale) 0 (alloy:pxx offset)
   0 (alloy:pxh scale) (alloy:pxy offset)))

(defun convert-image (image)
  (let ((size (simple:size image)))
    (raster:make-image (alloy:pxw size) (alloy:pxh size) (simple:data image))))

(defun convert-color (color)
  (raster:encode-color (round (* 255 (colored:r color)))
                       (round (* 255 (colored:g color)))
                       (round (* 255 (colored:b color)))
                       (round (* 255 (colored:a color)))))

(defun convert-stops (stops)
  (loop for (stop color) across stops
        collect (list stop (convert-color color))))

(defmethod (setf sampler) ((pattern simple:linear-gradient) (shape raster-shape))
  (let ((start (simple:start pattern))
        (stop (simple:stop pattern)))
    (setf (sampler shape) (raster:linear-gradient (convert-stops (simple:stops pattern))
                                                  (alloy:pxx start) (alloy:pxy start)
                                                  (alloy:pxx stop) (alloy:pxy stop)))))

(defmethod (setf sampler) ((pattern simple:radial-gradient) (shape raster-shape))
  (let ((start (simple:start pattern)))
    (setf (sampler shape) (raster:radial-gradient (convert-stops (simple:stops pattern))
                                                  (alloy:pxx start) (alloy:pxy start)))))

(defmethod (setf sampler) ((pattern simple:angle-gradient) (shape raster-shape))
  (let ((start (simple:start pattern)))
    (setf (sampler shape) (raster:conical-gradient (convert-stops (simple:stops pattern))
                                                   (alloy:pxx start) (alloy:pxy start)))))

(defmethod (setf sampler) ((pattern simple:diamond-gradient) (shape raster-shape))
  (let ((start (simple:start pattern)))
    (setf (sampler shape) (raster:diamond-gradient (convert-stops (simple:stops pattern))
                                                   (alloy:pxx start) (alloy:pxy start)))))

(defmethod alloy:register (renderable (renderer renderer)))

(defmethod simple:clip ((renderer renderer) (shape raster-shape))
  (setf raster::*clip* (raster:intersect raster::*clip* (sdf shape))))

(defmethod simple:clip ((renderer renderer) (extent alloy:extent))
  (let ((rect (raster:rectangle (alloy:pxx extent) (alloy:pxy extent) (alloy:pxw extent) (alloy:pxh extent))))
    (setf raster::*clip* (raster:intersect raster::*clip* rect))))

(defmethod simple:call-with-pushed-transforms :around (function (renderer renderer) &key clear)
  (declare (ignore clear))
  (let ((raster::*clip* raster::*clip*))
    (call-next-method)))

(defmethod simple:clear ((renderer renderer) (extent alloy:extent))
  (let ((x (alloy:pxx extent)) (y (alloy:pxy extent))
        (w (alloy:pxw extent)) (h (alloy:pxh extent)))
    ;; FIXME: this is not correct
    (destructuring-bind (bw . bh) (buffer-size renderer)
      (raster:draw-rectangle x y w h (buffer renderer) bw bh))))

(defmethod alloy:render ((renderer renderer) (shape simple:icon))
  (let* ((extent (alloy:ensure-extent (simple:bounds shape)))
         (x (alloy:pxx extent)) (y (alloy:pxy extent))
         (w (alloy:pxw extent)) (h (alloy:pxh extent))
         (scale (simple:size shape))
         (offset (simple:shift shape))
         (image (convert-image (simple:image shape))))
    ;; FIXME: consider transform matrix rotations properly
    (multiple-value-setq (x y) (simple:mat*p (simple:transform-matrix renderer) x y))
    (multiple-value-setq (w h) (simple:mat*v (simple:transform-matrix renderer) w h))
    (destructuring-bind (bw . bh) (buffer-size renderer)
      (raster:draw-image image x y (buffer renderer) bw bh
                         :w w :h h :transform (convert-transform scale offset)
                         :sizing (simple:sizing shape)
                         :halign (simple:halign shape)
                         :valign (simple:valign shape)))))

(defmethod simple:rectangle ((renderer renderer) bounds &rest initargs &key line-width)
  (apply #'make-instance (if line-width 'outlined-rectangle 'filled-rectangle) :bounds bounds initargs))

(defmethod simple:ellipse ((renderer renderer) bounds &rest initargs &key line-width)
  (apply #'make-instance (if line-width 'outlined-ellipse 'filled-ellipse) :bounds bounds initargs))

(defmethod simple:polygon ((renderer renderer) points &rest initargs &key)
  (apply #'make-instance 'polygon :points points initargs))

(defmethod simple:line-strip ((renderer renderer) points &rest initargs &key)
  (apply #'make-instance 'line-strip :points points initargs))

(defmethod simple:curve ((renderer renderer) points &rest initargs &key)
  (apply #'make-instance 'curve :points points initargs))

(defmethod simple:text ((renderer renderer) bounds string &rest initargs &key)
  (apply #'make-instance 'text :text string :bounds bounds :markup (simple:sort-markup (getf initargs :markup)) initargs))

(defclass outlined-rectangle (simple:outlined-rectangle raster-shape) ())

(defmethod alloy:render ((renderer renderer) (shape outlined-rectangle))
  (let* ((extent (alloy:ensure-extent (simple:bounds shape)))
         (x (alloy:pxx extent)) (y (alloy:pxy extent))
         (w (alloy:pxw extent)) (h (alloy:pxh extent))
         (lw (alloy:to-px (simple:line-width shape))))
    ;; FIXME: consider transform matrix rotations properly
    (multiple-value-setq (x y) (simple:mat*p (simple:transform-matrix renderer) x y))
    (multiple-value-setq (w h) (simple:mat*v (simple:transform-matrix renderer) w h))
    (destructuring-bind (bw . bh) (buffer-size renderer)
      (raster:draw-rectangle x y w h (buffer renderer) bw bh :sampler (sampler shape) :line-width lw))))

(defclass filled-rectangle (simple:filled-rectangle raster-shape) ())

(defmethod alloy:render ((renderer renderer) (shape filled-rectangle))
  (let* ((extent (alloy:ensure-extent (simple:bounds shape)))
         (x (alloy:pxx extent)) (y (alloy:pxy extent))
         (w (alloy:pxw extent)) (h (alloy:pxh extent)))
    ;; FIXME: consider transform matrix rotations properly
    (multiple-value-setq (x y) (simple:mat*p (simple:transform-matrix renderer) x y))
    (multiple-value-setq (w h) (simple:mat*v (simple:transform-matrix renderer) w h))
    (destructuring-bind (bw . bh) (buffer-size renderer)
      (raster:draw-rectangle x y w h (buffer renderer) bw bh :sampler (sampler shape)))))

(defclass outlined-ellipse (simple:outlined-ellipse raster-shape) ())

(defmethod alloy:render ((renderer renderer) (shape outlined-rectangle))
  (let* ((extent (alloy:ensure-extent (simple:bounds shape)))
         (x (alloy:pxx extent)) (y (alloy:pxy extent))
         (w (alloy:pxw extent)) (h (alloy:pxh extent))
         (lw (alloy:to-px (simple:line-width shape))))
    ;; FIXME: consider transform matrix rotations properly
    (multiple-value-setq (x y) (simple:mat*p (simple:transform-matrix renderer) x y))
    (multiple-value-setq (w h) (simple:mat*v (simple:transform-matrix renderer) w h))
    (destructuring-bind (bw . bh) (buffer-size renderer)
      (raster:draw-ellipse x y w h (buffer renderer) bw bh :sampler (sampler shape) :line-width lw :start (simple:start-angle shape) :end (simple:end-angle shape)))))

(defclass filled-ellipse (simple:filled-ellipse raster-shape) ())

(defmethod alloy:render ((renderer renderer) (shape filled-ellipse))
  (let* ((extent (alloy:ensure-extent (simple:bounds shape)))
         (x (alloy:pxx extent)) (y (alloy:pxy extent))
         (w (alloy:pxw extent)) (h (alloy:pxh extent)))
    ;; FIXME: consider transform matrix rotations properly
    (multiple-value-setq (x y) (simple:mat*p (simple:transform-matrix renderer) x y))
    (multiple-value-setq (w h) (simple:mat*v (simple:transform-matrix renderer) w h))
    (destructuring-bind (bw . bh) (buffer-size renderer)
      (raster:draw-ellipse x y w h (buffer renderer) bw bh :sampler (sampler shape) :start (simple:start-angle shape) :end (simple:end-angle shape)))))

(defclass polygon (simple:polygon raster-shape) ())

(defmethod alloy:render ((renderer renderer) (shape polygon))
  (let ((points (make-array (* 2 (length (simple:points shape))))))
    (declare (dynamic-extent points))
    (loop for point in (simple:points shape)
          for i from 0 by 2
          do (multiple-value-bind (x y) (simple:mat*p (simple:transform-matrix renderer) (alloy:pxx point) (alloy:pxy point))
               (setf (aref points (+ 0 i)) x)
               (setf (aref points (+ 1 i)) y)))
    (destructuring-bind (bw . bh) (buffer-size renderer)
      (raster:draw-polygon points (buffer renderer) bw bh :sampler (sampler shape)))))

(defclass line-strip (simple:line-strip raster-shape) ())

(defmethod alloy:render ((renderer renderer) (shape line-strip))
  (let ((points (make-array (* 2 (length (simple:points shape)))))
        (lw (alloy:to-px (simple:line-width shape))))
    (declare (dynamic-extent points))
    (loop for point in (simple:points shape)
          for i from 0 by 2
          do (multiple-value-bind (x y) (simple:mat*p (simple:transform-matrix renderer) (alloy:pxx point) (alloy:pxy point))
               (setf (aref points (+ 0 i)) x)
               (setf (aref points (+ 1 i)) y)))
    (destructuring-bind (bw . bh) (buffer-size renderer)
      (raster:draw-lines points (buffer renderer) bw bh
                         :sampler (sampler shape) :line-width lw
                         :line-style (simple:line-style shape)
                         :join-style (simple:join-style shape)
                         :cap-style (simple:cap-style shape)))))

(defclass curve (simple:curve raster-shape) ())

(defmethod alloy:render ((renderer renderer) (shape curve))
  (let ((points (make-array (* 2 (length (simple:points shape)))))
        (lw (alloy:to-px (simple:line-width shape))))
    (declare (dynamic-extent points))
    (loop for point in (simple:points shape)
          for i from 0 by 2
          do (multiple-value-bind (x y) (simple:mat*p (simple:transform-matrix renderer) (alloy:pxx point) (alloy:pxy point))
               (setf (aref points (+ 0 i)) x)
               (setf (aref points (+ 1 i)) y)))
    (destructuring-bind (bw . bh) (buffer-size renderer)
      (raster:draw-curves points (buffer renderer) bw bh
                         :sampler (sampler shape) :line-width lw
                         :line-style (simple:line-style shape)
                         :join-style (simple:join-style shape)
                         :cap-style (simple:cap-style shape)))))

(defclass text (simple:text raster-shape) ())

(defmethod alloy:render ((renderer renderer) (shape text)))
