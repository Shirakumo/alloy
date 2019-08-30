#|
 This file is a part of Alloy
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.alloy.renderers.simple)

;; FIXME: cache defaults
(defgeneric default-presentation (renderer thing focus))
(defgeneric render-presentation (renderer presentation component extent))
(defgeneric render-presentation-content (renderer presentation component extent))
(defgeneric merge-presentation (into from))
(defgeneric merge-presentation-into (into from))

(defclass presentation ()
  ((padding :initarg :padding :initform NIL :accessor padding)
   (background-color :initarg :background-color :initform NIL :accessor background-color)
   (border-color :initarg :border-color :initform NIL :accessor border-color)
   (border-thickness :initarg :border-thickness :initform NIL :accessor border-thickness)
   (text-color :initarg :text-color :initform NIL :accessor text-color)
   (text-alignment :initarg :text-alignment :initform NIL :accessor text-alignment)
   (text-vertical-alignment :initarg :text-vertical-alignment :initform NIL :accessor text-vertical-alignment)
   (text-direction :initarg :text-direction :initform NIL :accessor text-direction)
   (text-size :initarg :text-size :initform NIL :accessor text-size)
   (font-family :initarg :font-family :initform NIL :accessor font-family)
   (font-slant :initarg :font-slant :initform NIL :accessor font-slant)
   (font-spacing :initarg :font-spacing :initform NIL :accessor font-spacing)
   (font-weight :initarg :font-weight :initform NIL :accessor font-weight)
   (font-stretch :initarg :font-stretch :initform NIL :accessor font-stretch)
   ;; FIXME: rethink this.
   (image-size :initarg :image-size :initform NIL :accessor image-size)
   (image-fill :initarg :image-fill :initform NIL :accessor image-fill)
   (image-alignment :initarg :image-alignment :initform NIL :accessor image-alignment)))

(defmethod merge-presentation ((into presentation) (from presentation))
  (merge-presentation-into
   (merge-presentation-into
    (make-instance 'presentation)
    into)
   from))

(defmethod merge-presentation-into ((into presentation) (from presentation))
  (macrolet ((%merge (&rest accessors)
               `(progn
                  ,@(loop for accessor in accessors
                          collect `(unless (,accessor into)
                                     (setf (,accessor into) (,accessor from)))))))
    (%merge padding background-color border-color border-thickness
            text-color text-alignment text-vertical-alignment text-direction text-size
            font-family font-slant font-spacing font-weight font-stretch
            image-size image-fill image-alignment)
    into))

(defclass presentable-layout-element (alloy:layout-element)
  ((presentation :initarg :presentation :initform (error "PRESENTATION required.") :accessor presentation)))

(defclass presentable-layout-entry (alloy:layout-entry presentable-layout-element)
  ())

(defmethod alloy:render-with ((renderer renderer) (element alloy:layout-element) thing)
  (render-presentation renderer
                       (default-presentation renderer thing (alloy:focus-for thing renderer))
                       thing
                       (alloy:bounds element)))

(defmethod alloy:render-with ((renderer renderer) (element presentable-layout-element) thing)
  (render-presentation renderer
                       (merge-presentation
                        (presentation element)
                        (default-presentation renderer thing (alloy:focus-for thing renderer)))
                       thing
                       (alloy:bounds element)))

(defmethod render-presentation ((renderer renderer)
                                (presentation presentation)
                                thing
                                extent)
  (when (background-color presentation)
    (setf (fill-color renderer) (background-color presentation))
    (setf (fill-mode renderer) :fill)
    (rectangle renderer extent))
  (when (border-color presentation)
    (setf (fill-color renderer) (border-color presentation))
    (setf (line-width renderer) (border-thickness presentation))
    (setf (fill-mode renderer) :lines)
    (rectangle renderer (alloy:extent (+ (alloy:extent-x extent) (/ (border-thickness presentation) 2))
                                      (+ (alloy:extent-y extent) (/ (border-thickness presentation) 2))
                                      (- (alloy:extent-w extent) (border-thickness presentation))
                                      (- (alloy:extent-h extent) (border-thickness presentation))))))

(defmethod render-presentation :after ((renderer renderer)
                                       (presentation presentation)
                                       thing
                                       extent)
  (let ((padding (padding presentation)))
    (with-pushed-transforms (renderer)
      (let ((inner (alloy:extent (+ (alloy:extent-x extent) (alloy:margins-l padding) (border-thickness presentation))
                                 (+ (alloy:extent-y extent) (alloy:margins-b padding) (border-thickness presentation))
                                 (- (alloy:extent-w extent) (alloy:margins-l padding) (alloy:margins-r padding) (* 2 (border-thickness presentation)))
                                 (- (alloy:extent-h extent) (alloy:margins-b padding) (alloy:margins-u padding) (* 2 (border-thickness presentation))))))
        (clip renderer inner)
        (render-presentation-content renderer presentation thing inner)))))

(defmethod render-presentation-content ((renderer renderer) (presentation presentation) (component alloy:component) extent))

(defun align-point (direction halign valign extent)
  (alloy:point
   (+ (alloy:extent-x extent)
      (ecase halign
        (:start
         (ecase direction
           (:right 0)
           ((:left :down) (alloy:extent-w extent))))
        (:middle
         (/ (alloy:extent-w extent) 2))
        (:end
         (ecase direction
           (:right (alloy:extent-w extent))
           ((:left :down) 0)))))
   (+ (alloy:extent-y extent)
      (ecase valign
        (:top (alloy:extent-h extent))
        (:middle (/ (alloy:extent-h extent) 2))
        (:bottom 0)))))

(defmethod render-presentation-content ((renderer renderer) (presentation presentation) (component alloy:text-component) extent)
  (call-next-method)
  (when (and (text-color presentation)
             (alloy:text component))
    (setf (fill-color renderer) (text-color presentation))
    (let ((font (request-font
                 renderer
                 (make-instance 'font :family (font-family presentation)
                                      :slant (font-slant presentation)
                                      :spacing (font-spacing presentation)
                                      :weight (font-weight presentation)
                                      :stretch (font-stretch presentation))))
          ;; FIXME: proper alignment in presence of image
          (point (align-point (text-direction presentation)
                              (text-alignment presentation)
                              (text-vertical-alignment presentation)
                              extent)))
      (text renderer point (alloy:text component)
            :align (text-alignment presentation)
            :vertical-align (text-vertical-alignment presentation)
            :direction (text-direction presentation)
            :size (text-size presentation)
            :font font))))

(defmethod render-presentation-content ((renderer renderer) (presentation presentation) (component alloy:image-component) extent)
  (call-next-method)
  (when (and (image-size presentation)
             (alloy:image component))
    ;; FIXME: proper alignment and direction resolution
    (let ((point extent))
      (image renderer point (alloy:image component)
             :size (image-size presentation)))))

(defmethod render-presentation-content ((renderer renderer) (presentation presentation) (component alloy:switch) extent)
  (with-pushed-transforms (renderer)
    (setf (fill-mode renderer) :fill)
    (setf (fill-color renderer) (text-color presentation))
    (rectangle renderer (alloy:extent (+ (alloy:extent-x extent)
                                         (if (alloy:state component)
                                             (- (alloy:extent-w extent) (/ (alloy:extent-w extent) 2.5))
                                             0))
                                      (alloy:extent-y extent)
                                      (/ (alloy:extent-w extent) 2.5)
                                      (alloy:extent-h extent)))))

(defmethod render-presentation-content ((renderer renderer) (presentation presentation) (component alloy:icon) extent)
  (let ((point extent))
    (image renderer point (alloy:image component)
           :size (alloy:size (alloy:extent-w extent) (alloy:extent-h extent)))))

;;; LAF protocol

(defclass look-and-feel ()
  ())

(defclass look-and-feel-renderer (renderer)
  ((look-and-feel :initarg :look-and-feel :accessor look-and-feel)))

(defmethod default-presentation ((renderer look-and-feel-renderer) component focus)
  (default-presentation (look-and-feel renderer) component focus))
