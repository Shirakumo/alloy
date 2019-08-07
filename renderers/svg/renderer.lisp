#|
 This file is a part of Alloy
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.alloy.renderers.svg)

(defvar *container*)

(defclass image (simple:image)
  ())

(defmethod ->svg ((thing simple:color))
  (format NIL "rgb(~d, ~d, ~d)"
          (floor (* 255 (simple:r thing)))
          (floor (* 255 (simple:g thing)))
          (floor (* 255 (simple:b thing)))))

(defmethod ->svg ((thing vector))
  (format NIL "matrix(~f, ~f, ~f, ~f, ~f, ~f)"
          (aref thing 0) (aref thing 3)
          (aref thing 1) (aref thing 4)
          (aref thing 2) (aref thing 5)))

(defmethod ->svg ((thing float))
  (format NIL "~fpx" thing))

(defmethod ->svg ((thing alloy:extent))
  (format NIL "rect(~dpx, ~dpx, ~dpx, ~dpx)"
          (- (alloy:h *container*) (alloy:y thing) (alloy:h thing))
          (- (alloy:w *container*) (alloy:x thing) (alloy:w thing))
          (alloy:y thing)
          (alloy:x thing)))

(defgeneric svg-parameters (thing)
  (:method-combination append))

(defmethod svg-parameters append ((style simple:style))
  (list*
   :opacity (simple:a (simple:fill-color style))
   (ecase (simple:fill-mode style)
     (:lines
      (list :stroke (->svg (simple:fill-color style))
            :stroke-width (->svg (simple:line-width style))
            :style "fill:none;"))
     (:fill
      (list :fill (->svg (simple:fill-color style)))))))

(defmethod svg-parameters append ((transform simple:transform))
  (list*
   :transform (->svg (simple:transform-matrix transform))
   (when (simple:clip-mask transform)
     (list :clip (->svg (simple:clip-mask transform))))))

(defclass renderer (simple:simple-styled-renderer
                    simple:simple-transformed-renderer)
  ((scene :accessor scene)
   (size :initarg :size :initform (alloy:size 800 600) :accessor size)))

(defmethod alloy:h ((renderer renderer))
  (alloy:h (size renderer)))

(defmethod alloy:w ((renderer renderer))
  (alloy:w (size renderer)))

(defmethod alloy:allocate ((renderer renderer))
  (unless (slot-boundp renderer 'scene)
    (setf (scene renderer) (cl-svg:make-svg-toplevel 'cl-svg:svg-1.1-toplevel
                                                     :width (alloy:w (size renderer))
                                                     :height (alloy:h (size renderer))))))

(defmethod alloy:deallocate ((renderer renderer))
  (slot-makunbound renderer 'scene))

(defmethod alloy:register (renderable (renderer renderer)))

(defmethod alloy:render :around ((renderer renderer) (ui alloy:ui))
  (let ((*container* (scene renderer)))
    (call-next-method)))

(defmethod render-to-file (renderable file (renderer renderer) &key (if-exists :supersede))
  (with-open-file (stream file :direction :output
                               :if-exists if-exists)
    (alloy:allocate renderer)
    (unwind-protect
         (progn
           (when renderable
             (alloy:render renderer renderable))
           (cl-svg:stream-out stream (scene renderer))
           file)
      (alloy:deallocate renderer))))

(defmethod draw ((renderer renderer) type &rest parameters)
  (cl-svg:add-element
   (scene *container*)
   (cl-svg::make-svg-element
    type
    (append parameters
            (svg-parameters (simple:style renderer))
            (svg-parameters (simple:transform renderer))))))

(defmethod simple:line ((renderer renderer) point-a point-b)
  (draw renderer :line
        :x1 (alloy:x point-a)
        :y1 (- (alloy:h renderer) (alloy:y point-a))
        :x2 (alloy:x point-b)
        :y2 (- (alloy:h renderer) (alloy:y point-b))))

(defmethod simple:rectangle ((renderer renderer) extent)
  (draw renderer :rect
        :x (alloy:x extent)
        :y (- (alloy:h renderer) (alloy:y extent) (alloy:h extent))
        :width (alloy:w extent)
        :height (alloy:h extent)))

(defmethod simple:ellipse ((renderer renderer) extent)
  (draw renderer :ellipse
        :cx (+ (alloy:x extent) (/ (alloy:w extent) 2))
        :cy (- (alloy:h renderer) (alloy:y extent) (/ (alloy:h extent) 2))
        :rx (/ (alloy:w extent) 2)
        :ry (/ (alloy:h extent) 2)))

(defmethod simple:polygon ((renderer renderer) points)
  (draw renderer :polygon
        :points (with-output-to-string (out)
                  (dolist (point points)
                    (format out "~a,~a "
                            (alloy:x point)
                            (- (alloy:h renderer) (alloy:y point)))))))

(defmethod simple:text ((renderer renderer) point string &key (font (simple:font renderer))
                                                              (size (simple:font-size renderer)))
  (cl-svg:text (scene renderer)
      (:x (alloy:x point)
       :y (- (alloy:h renderer) (alloy:y point))
       :color (->svg (simple:fill-color renderer))
       :opacity (simple:a (simple:fill-color renderer))
       :font-family (simple:family font)
       :font-style (simple:style font)
       :font-variant (simple:variant font)
       :font-weight (simple:weight font)
       :font-stretch (simple:stretch font)
       :font-size size)
    string))

(defmethod simple:image ((renderer renderer) point image &key (size (size image)))
  (draw renderer :image
        :x (alloy:x point)
        :y (- (alloy:h renderer) (alloy:y point))
        :width (alloy:w size)
        :height (alloy:h size)
        :xlink-href (data image)))

(defmethod simple:clear ((renderer renderer) extent)
  ;; DUMB
  (draw renderer :rect
        :x (alloy:x extent)
        :y (- (alloy:h renderer) (alloy:y extent) (alloy:h extent))
        :width (alloy:w extent)
        :height (alloy:h extent)
        :fill "white"))


(defmethod simple:request-font ((renderer renderer) (font simple:font))
  font)

(defmethod simple:request-image ((renderer renderer) (image simple:image))
  ;; FIXME: implement
  (make-instance 'image :size (simple:size image)
                        :data (format NIL "data:image/bmp;base64,")))
