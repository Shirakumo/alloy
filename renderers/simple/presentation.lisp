#|
 This file is a part of Alloy
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.alloy.renderers.simple)

;; FIXME: cache defaults
(defgeneric default-presentation (renderer thing focus))
(defgeneric render-presentation (renderer presentation component extent))
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
   (font-style :initarg :font-style :initform NIL :accessor font-style)
   (font-variant :initarg :font-variant :initform NIL :accessor font-variant)
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
            font-family font-style font-variant font-weight font-stretch
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

(defun align-point (direction halign valign padding extent)
  (alloy:point
   (+ (alloy:extent-x extent)
      (ecase halign
        (:start
         (ecase direction
           (:right (alloy:margins-l padding))
           ((:left :down) (- (alloy:extent-w extent) (alloy:margins-r padding)))))
        (:middle
         (+ (alloy:margins-l padding)
            (/ (- (alloy:extent-w extent) (alloy:margins-l padding) (alloy:margins-l padding))
               2)))
        (:end
         (ecase direction
           (:right (- (alloy:extent-w extent) (alloy:margins-r padding)))
           ((:left :down) (alloy:margins-l padding))))))
   (+ (alloy:extent-y extent)
      (ecase valign
        (:top
         (- (alloy:extent-h extent) (alloy:margins-u padding)))
        (:middle
         (+ (alloy:margins-b padding)
            (/ (- (alloy:extent-h extent) (alloy:margins-b padding) (alloy:margins-u padding))
               2)))
        (:bottom
         (alloy:margins-b padding))))))

(defmethod render-presentation ((renderer renderer)
                                (presentation presentation)
                                (component alloy:text-component)
                                extent)
  (call-next-method)
  (when (and (text-color presentation)
             (alloy:text component))
    (setf (fill-color renderer) (text-color presentation))
    (let ((font (request-font
                 renderer
                 (make-instance 'font :family (font-family presentation)
                                      :style (font-style presentation)
                                      :variant (font-variant presentation)
                                      :weight (font-weight presentation)
                                      :stretch (font-stretch presentation))))
          ;; FIXME: proper alignment in presence of image
          (point (align-point (text-direction presentation)
                              (text-alignment presentation)
                              (text-vertical-alignment presentation)
                              (padding presentation)
                              extent)))
      (with-pushed-transforms (renderer)
        (clip renderer extent)
        (text renderer point (alloy:text component)
          :align (text-alignment presentation)
          :vertical-align (text-vertical-alignment presentation)
          :direction (text-direction presentation)
          :size (text-size presentation)
          :font font)))))

(defmethod render-presentation ((renderer renderer)
                                (presentation presentation)
                                (component alloy:image-component)
                                extent)
  (call-next-method)
  (when (and (image-size presentation)
             (alloy:image component))
    ;; FIXME: proper alignment and direction resolution
    (let ((point extent))
      (image renderer point (alloy:image component)
             :size (image-size presentation)))))

;;; LAF protocol

(defclass look-and-feel ()
  ())

(defclass look-and-feel-renderer (renderer)
  ((look-and-feel :initarg :look-and-feel :accessor look-and-feel)))

(defmethod default-presentation ((renderer look-and-feel-renderer) component focus)
  (default-presentation (look-and-feel renderer) component focus))

