#|
 This file is a part of Alloy
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.alloy.renderers.simple)

(defclass font ()
  ((family :initarg :family :initform (arg! :family) :reader family)
   (slant :initarg :slant :initform :roman :reader slant)
   (spacing :initarg :spacing :initform :proportional :reader spacing)
   (weight :initarg :weight :initform :regular :reader weight)
   (stretch :initarg :stretch :initform :normal :reader stretch)))

(defmethod request-font ((renderer renderer) (family string) &rest initargs)
  (apply #'make-instance 'font :family family initargs))

(defmethod print-object ((font font) stream)
  (print-unreadable-object (font stream :type T)
    (format stream "~a" (family font))))

(defclass image ()
  ((size :initarg :size :reader size)
   (data :initarg :data :reader data)
   (channels :initarg :channels :reader channels)))

(defmethod request-image ((renderer renderer) (data vector) &rest initargs)
  (apply #'make-instance 'image :data data initargs))

(defmethod alloy:enter ((image image) (layout alloy:layout) &rest args)
  (apply #'alloy:enter (alloy:represent-with 'alloy:icon image) layout args))

(defmethod alloy:component-class-for-object ((image image))
  (find-class 'alloy:icon))

(defclass gradient ()
  ((start :initarg :start :initform (arg! :start) :reader start)
   (stop :initarg :stop :initform (arg! :stop) :reader stop)
   (stops :initarg :stops :initform (arg! :stops) :reader stops)))

(defclass linear-gradient (gradient) ())
(defclass radial-gradient (gradient) ())
(defclass angle-gradient (gradient) ())
(defclass diamond-gradient (gradient) ())

(defmethod request-gradient ((renderer renderer) type start stop stops &rest initargs)
  (apply #'make-instance type :start start :stop stop :stops stops initargs))

(defclass image-pattern ()
  ((image :initarg :image :initform (arg! :image) :reader image)
   (scaling :initarg :scaling :initform (alloy:size 1 1) :reader scaling)
   (offset :initarg :offset :initform (alloy:point 0 0) :reader offset)
   (mode :initarg :mode :initform :repeat :reader mode)))

(defmethod image-pattern ((renderer renderer) image &rest initargs)
  (apply #'make-instance 'image-pattern :image image initargs))

(defclass shape () ())

(defmethod reinitialize-instance :after ((shape shape) &key renderer)
  (declare (ignore renderer)))

(defclass patterned-shape (shape)
  ((pattern :initarg :pattern :initform colors:black :accessor pattern)))

(defclass filled-shape (patterned-shape) ())

(defclass outlined-shape (patterned-shape)
  ((line-width :initarg :line-width :initform (alloy:un 1) :accessor line-width)
   (line-style :initarg :line-style :initform NIL :accessor line-style)
   (join-style :initarg :join-style :initform NIL :accessor join-style)
   (cap-style :initarg :cap-style :initform NIL :accessor cap-style)))

(defclass rectangle (shape)
  ((bounds :initarg :bounds :initform (arg! :bounds) :accessor bounds)
   (corner-radius :initarg :corner-radius :initform (make-array 4 :element-type 'single-float) :accessor corner-radius)))

(defclass filled-rectangle (rectangle filled-shape) ())
(defclass outlined-rectangle (rectangle outlined-shape) ())

(defmethod rectangle ((renderer renderer) bounds &rest initargs &key line-width)
  (apply #'make-instance (if line-width 'outlined-rectangle 'filled-rectangle) :bounds bounds initargs))

(defclass ellipse (shape)
  ((bounds :initarg :bounds :initform (arg! :bounds) :accessor bounds)
   (start-angle :initarg :start-angle :initform 0.0 :accessor start-angle)
   (end-angle :initarg :end-angle :initform #.(float (* 2 PI) 0f0) :accessor end-angle)))

(defmethod (setf start-angle) ((value real) (ellipse ellipse))
  (setf (slot-value ellipse 'start-angle) (mod (float value 0f0) (float (* 2 PI) 0f0))))

(defmethod (setf end-angle) ((value real) (ellipse ellipse))
  (setf (slot-value ellipse 'end-angle) (mod (float value 0f0) (float (* 2 PI) 0f0))))

(defclass filled-ellipse (ellipse filled-shape) ())
(defclass outlined-ellipse (ellipse outlined-shape) ())

(defmethod ellipse ((renderer renderer) bounds &rest initargs &key line-width)
  (apply #'make-instance (if line-width 'outlined-ellipse 'filled-ellipse) :bounds bounds initargs))

(defclass polygon (filled-shape)
  ((points :initarg :points :initform (arg! :points) :accessor points)))

(defmethod polygon ((renderer renderer) points &rest initargs)
  (apply #'make-instance 'polygon :points points initargs))

(defclass line-strip (outlined-shape)
  ((points :initarg :points :initform (arg! :points) :accessor points)))

(defmethod line-strip ((renderer renderer) points &rest initargs)
  (apply #'make-instance 'line-strip :points points initargs))

(defclass curve (outlined-shape)
  ((points :initarg :points :initform (arg! :points) :accessor points)))

(defmethod curve ((renderer renderer) points &rest initargs)
  (apply #'make-instance 'curve :points points initargs))

;; TODO: changing styles and size, multiple styles and size per text
;;       Might be too much for a simple interface though. Maybe could be
;;       done by a layouter -- splitting text into contiguous segments.
;;       would be difficult to manage with line wraps and bidi.
(defun sort-markup (markup)
  (flet ((sorter (a b)
           (let ((sa (pop a)) (ea (pop a))
                 (sb (pop b)) (eb (pop b)))
             (if (= sa sb)
                 (< eb ea)
                 (< sa sb)))))
    (sort markup #'sorter)))

(defun flatten-markup (markup)
  (let ((results ())
        (styles ()))
    ;; We do a dual-stack approach: the markup has to be sorted in sequence
    ;; without overlaps. We then scan through this sequence by iterating an
    ;; index and matching against possible starting markups (second WHEN).
    ;; If we match we push the end indices of matching styles to STYLES as
    ;; well as a singular matching index for the start of the matching
    ;; STYLE. As we iterate and appear at the matching indices of the STYLES
    ;; stack we can then actually push them onto the results list.
    (loop for i from 0
          while markup
          do (when (and styles (= i (caar styles)))
               (pop styles)
               (push (list* i (mapcar #'cdr styles)) results))
             (when (= i (caar markup))
               (loop while (and markup (= i (caar markup)))
                     do (destructuring-bind (s e style) (pop markup)
                          (declare (ignore s))
                          (push (cons e style) styles)))
               (push (list* i (mapcar #'cdr styles)) results)))
    ;; Now we process any remaining dangling styles.
    (loop for style = (pop styles)
          while style
          do (push (list* (car style) (mapcar #'cdr styles)) results))
    ;; Finally we need to ignore successive styles that begin with the
    ;; same index and only use the first (ultimately last) one, as it
    ;; would override all others anyway.
    (when results
      (let ((rresults (list (pop results))))
        (dolist (style results rresults)
          (unless (= (car style) (caar rresults))
            (push style rresults)))))))

(defclass text (shape)
  ((alloy:text :initarg :text :initform (arg! :text) :accessor alloy:text)
   (font :initarg :font :initform (arg! :font) :accessor font)
   (pattern :initarg :pattern :initform colors:black :accessor pattern)
   (size :initarg :size :initform (alloy:un 12) :accessor size)
   (bounds :initarg :bounds :initform (alloy:margins) :accessor bounds)
   (valign :initarg :valign :initform :bottom :accessor valign)
   (halign :initarg :halign :initform :start :accessor halign)
   (direction :initarg :direction :initform :right :accessor direction)
   (wrap :initarg :wrap :initform NIL :accessor wrap)
   (markup :initarg :markup :initform NIL :accessor markup)))

(defmethod text ((renderer renderer) bounds (string string) &rest initargs)
  (apply #'make-instance 'text :text string :bounds bounds :markup (sort-markup (getf initargs :markup)) initargs))

(defmethod text :around ((renderer renderer) bounds text &rest initargs &key font)
  (let ((font (typecase font
                (font font)
                (null (request-font renderer :default))
                (T (request-font renderer font)))))
    (apply #'call-next-method renderer bounds text :font font initargs)))

(defmethod reinitialize-instance :after ((text text) &key (font NIL font-p) renderer)
  (when (and font-p (not (typep font 'font)))
    (setf (font text) (request-font renderer font))))

(defmethod (setf markup) :around ((markup cons) (text text))
  (call-next-method (sort-markup markup) text))

(defmethod alloy:render :before ((renderer renderer) (text text))
  (alloy:constrain-visibility (alloy:ensure-extent (bounds text)) renderer))

(defclass icon (shape)
  ((image :initarg :image :initform (arg! :image) :accessor image)
   (size :initarg :size :initform (alloy:px-size 1 1) :accessor size)
   (shift :initarg :shift :initform (alloy:px-point 0 0) :accessor shift)
   (bounds :initarg :bounds :initform (alloy:margins) :accessor bounds)
   (sizing :initarg :sizing :initform :fit :accessor sizing)
   (valign :initarg :valign :initform :middle :accessor valign)
   (halign :initarg :halign :initform :left :accessor halign)))

(defmethod icon ((renderer renderer) bounds (image image) &rest initargs)
  (apply #'make-instance 'icon :image image :bounds bounds initargs))

(defmethod reinitialize-instance :after ((icon icon) &key (image NIL image-p) renderer)
  (when (and image-p (not (typep image 'image)))
    (setf (image icon) (request-image renderer image))))

(defclass cursor (filled-rectangle)
  ((text-object :initarg :text :accessor text-object)
   (start :initarg :start :accessor start)
   (bounds :initform NIL)))

(defmethod cursor ((renderer renderer) (text text) (start integer) &rest initargs)
  (apply #'make-instance 'cursor :text text :start start initargs))

(defclass selection (polygon)
  ((text-object :initarg :text :accessor text-object)
   (start :initarg :start :accessor start)
   (end :initarg :end :accessor end)
   (pattern :initform (colored:color 0 0 1 0.5))
   (points :initform NIL)))

(defmethod selection ((renderer renderer) (text text) (start integer) (end integer) &rest initargs)
  (apply #'make-instance 'selection :text text :start start :end end initargs))

(defun resolve-alignment (extent halign valign size)
  (let ((extent (alloy:ensure-extent extent)))
    (alloy:px-point
     (+ (alloy:pxx extent)
        (ecase halign
          ((:start :left) 0)
          (:middle (/ (- (alloy:pxw extent) (alloy:pxw size)) 2))
          ((:end :right) (- (alloy:pxw extent) (alloy:pxw size)))))
     (+ (alloy:pxy extent)
        (ecase valign
          (:bottom 0)
          (:middle (/ (- (alloy:pxh extent) (alloy:pxh size)) 2))
          (:top (- (alloy:pxh extent) (alloy:pxh size))))))))

(defun resolve-scale (extent size kind)
  (let ((ow (alloy:pxw extent))
        (oh (alloy:pxh extent))
        (iw (alloy:pxw size))
        (ih (alloy:pxh size)))
    (ecase kind
      (:fit
       extent)
      (:fit-width
       (alloy:px-size ow (* (/ ow iw) ih)))
      (:fit-height
       (alloy:px-size (* (/ oh ih) iw) oh))
      (:contain
       (let ((s (min (/ ow iw) (/ oh ih))))
         (alloy:px-size (* s iw) (* s ih))))
      (:cover
       (let ((s (max (/ ow iw) (/ oh ih))))
         (alloy:px-size (* s iw) (* s ih)))))))
