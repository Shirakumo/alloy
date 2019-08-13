#|
 This file is a part of Alloy
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage #:org.shirakumo.alloy.renderers.opengl.fond
  (:use #:cl)
  (:local-nicknames
   (#:alloy #:org.shirakumo.alloy)
   (#:simple #:org.shirakumo.alloy.renderers.simple)
   (#:opengl #:org.shirakumo.alloy.renderers.opengl))
  (:export
   #:*default-charset*
   #:renderer
   #:font
   #:charset
   #:base-size))
(in-package #:org.shirakumo.alloy.renderers.opengl.fond)

(defparameter *default-charset*
  " !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~
¡¢£¤¥¦§¨©ª«¬­®¯°±²³´µ¶·¸¹º»¼½¾¿ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖ×ØÙÚÛÜÝÞßàáâãäåæçèéêëìíîïðñòóôõö÷øùúûüýþÿ")

(defclass renderer (opengl:renderer)
  ())

(defmethod alloy:allocate :before ((renderer renderer))
  (setf (opengl:resource 'text-vbo renderer)
        (opengl:make-vertex-buffer renderer (make-array 0 :element-type 'single-float)
                                   :data-usage :dynamic-draw))
  (setf (opengl:resource 'text-ebo renderer)
        (opengl:make-vertex-buffer renderer (make-array 0 :element-type 'single-float)
                                   :buffer-type :element-array-buffer
                                   :data-usage :dynamic-draw))
  (setf (opengl:resource 'text-vao renderer)
        (opengl:make-vertex-array renderer `((,(opengl:resource 'text-vbo renderer) :size 2 :stride 16 :offset 0)
                                    (,(opengl:resource 'text-vbo renderer) :size 2 :stride 16 :offset 8)
                                    ,(opengl:resource 'text-ebo renderer))))
  (setf (opengl:resource 'text-shader renderer)
        (opengl:make-shader renderer
                            :vertex-shader "#version 330 core
layout (location=0) in vec3 pos;
layout (location=1) in vec2 in_uv;
out vec2 uv;

uniform mat3 transform;

void main(){
  gl_Position = vec4(transform*vec3(pos.xy,1), 1);
  uv = in_uv;
}"
                            :fragment-shader "#version 330 core
in vec2 uv;
out vec4 out_color;

uniform sampler2D image;
uniform vec4 color;

void main(){
  out_color = color * texture(image, uv, -0.65).r;
}")))

(defclass font (simple:font)
  ((atlas :accessor atlas)
   (charset :initarg :charset :initform *default-charset* :reader charset)
   (base-size :initarg :base-size :initform 24 :reader base-size)))

(defmethod alloy:allocate ((font font))
  (setf (atlas font) (cl-fond:make-font (simple:family font) (charset font) :size (base-size font))))

(defmethod alloy:deallocate ((font font))
  (cl-fond:free (atlas font)))

(defmethod simple:request-font ((renderer renderer) (fontspec string))
  (simple:request-font renderer (make-instance 'font :family fontspec)))

(defmethod simple:request-font ((renderer renderer) (fontspec pathname))
  (simple:request-font renderer (make-instance 'font :family fontspec)))

(defmethod simple:request-font ((renderer renderer) (font simple:font))
  (cond ((string= "" (simple:family font))
         (simple:request-font renderer :default))
        ((stringp (simple:family font))
         ;; FIXME: load up system fonts
         (error "Don't know how to load system fonts."))
        (T
         (change-class font 'font))))

(defmethod simple:request-font ((renderer renderer) (font font))
  font)

(defun text-point (point atlas text align direction vertical-align)
  (destructuring-bind (&key l r ((:t u)) b gap) (cl-fond:compute-extent atlas text)
    (declare (ignore b gap))
    (ecase direction
      (:right
       (alloy:point
        (ecase align
          (:start
           (alloy:x point))
          (:middle
           (- (alloy:x point) (/ (- r l) 2)))
          (:end
           (- (alloy:x point) (- r l))))
        (ecase vertical-align
          (:bottom
           (alloy:y point))
          (:middle
           (- (alloy:y point) (/ u 2)))
          (:top
           (- (alloy:y point) u))))))))

(defmethod simple:text ((renderer renderer) point string &key (font (simple:font renderer))
                                                              (size (simple:font-size renderer))
                                                              (align :start)
                                                              (direction :right)
                                                              (vertical-align :bottom))
  (let ((atlas (atlas font))
        (shader (opengl:resource 'text-shader renderer)))
    (opengl:bind shader)
    (gl:bind-texture :texture-2d (cl-fond:texture atlas))
    (simple:with-pushed-transforms (renderer)
      (simple:translate renderer (text-point point atlas string align direction vertical-align))
      (let ((s (/ size (cl-fond:size atlas))))
        (simple:scale renderer (alloy:size s s)))
      (setf (opengl:uniform shader "transform") (simple:transform-matrix (simple:transform renderer))))
    (setf (opengl:uniform shader "color") (simple:fill-color renderer))
    (let ((count (cl-fond:update-text atlas string
                                      (opengl:gl-name (opengl:resource 'text-vbo renderer))
                                      (opengl:gl-name (opengl:resource 'text-ebo renderer)))))
      (opengl:draw-vertex-array (opengl:resource 'text-vao renderer) :triangles count))))
