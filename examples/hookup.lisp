(defpackage #:org.shirakumo.alloy.examples.hookup
  (:use #:cl)
  (:local-nicknames
   (#:glfw #:org.shirakumo.fraf.glfw)
   (#:alloy #:org.shirakumo.alloy)
   (#:animation #:org.shirakumo.alloy.animation)
   (#:opengl #:org.shirakumo.alloy.renderers.opengl)
   (#:simple #:org.shirakumo.alloy.renderers.simple)
   (#:presentations #:org.shirakumo.alloy.renderers.simple.presentations))
  (:export
   #:launch))

(in-package #:org.shirakumo.alloy.examples.hookup)

(defclass renderer (org.shirakumo.alloy.renderers.opengl.msdf:renderer
                    org.shirakumo.alloy.renderers.opengl.png:renderer
                    presentations:default-look-and-feel)
  ())

(defmethod alloy:allocate ((renderer renderer)))

(defclass ui (alloy:fixed-scaling-ui)
  ((alloy:target-resolution :initform (alloy:px-size 1280 720))
   (alloy:scales :initform '((3840 T 2.0)
                             (2800 T 1.5)
                             (1920 T 1.25)
                             (1280 T 1.0)
                             (1000 T 0.8)
                             (T T 0.5)))))

;; STUB
(defmethod (setf alloy:cursor) (cursor (ui ui)))

(defclass window (glfw:window renderer)
  ((ui :initform (make-instance 'ui) :accessor ui)
   (cursor-location :initform (alloy:px-point 0 0) :accessor cursor-location)))

(defmethod initialize-instance :after ((window window) &key)
  (alloy:allocate window)
  ;; Setup initial GL state
  (gl:clear-color 0 0 0 0)
  (gl:enable :blend :depth-test :depth-clamp :stencil-test)
  (gl:clear-stencil #x00)
  (gl:stencil-func :always 1 #xFF)
  (gl:stencil-mask #xFF)
  (gl:clear-depth 0.0)
  (gl:depth-func :gequal)
  (gl:depth-mask T)
  (gl:blend-func :src-alpha :one-minus-src-alpha)
  ;; Construct our test layout
  (let ((layout (make-instance 'alloy:grid-layout :col-sizes '(100 T) :row-sizes '(30) :layout-parent (alloy:layout-tree (ui window))))
        (focus (make-instance 'alloy:focus-list :focus-parent (alloy:focus-tree (ui window)))))
    (alloy:enter "Hello" layout)
    (let ((button (alloy:represent "Test" 'alloy:button :layout-parent layout :focus-parent focus)))
      (alloy:on alloy:activate (button)
        (print :HELLO)))))

;; Hook up events
(defmethod glfw:window-resized ((window window) w h)
  (alloy:suggest-size (alloy:px-size w h) (ui window))
  (let ((target (simple:transform-matrix window)))
    (setf (aref target 0) (/ 2f0 (max 1f0 w)))
    (setf (aref target 1) 0f0)
    (setf (aref target 2) -1f0)

    (setf (aref target 3) 0f0)
    (setf (aref target 4) (/ 2f0 (max 1f0 h)))
    (setf (aref target 5) -1f0)

    (setf (aref target 6) 0f0)
    (setf (aref target 7) 0f0)
    (setf (aref target 8) 0.0001f0)
    (setf (simple:identity-matrix window) (copy-seq target))))

(defmethod glfw:mouse-moved ((window window) x y)
  (let ((location (alloy:px-point x (- (second (glfw:size window)) y))))
    (alloy:handle (make-instance 'alloy:pointer-move
                                 :old-location (cursor-location window)
                                 :location location)
                  (ui window))
    (setf (cursor-location window) location)))

(defmethod glfw:mouse-button-changed ((window window) button action mods)
  (case action
    (:press
     (alloy:handle (make-instance 'alloy:pointer-down
                                              :location (cursor-location window)
                                              :kind button)
                   (ui window)))
    (:release
     (alloy:handle (make-instance 'alloy:pointer-up
                                              :location (cursor-location window)
                                              :kind button)
                   (ui window)))))

(defmethod glfw:key-changed ((window window) key code action mods)
  (case action
    (:press
     (alloy:handle (make-instance 'alloy:key-down :code code :key key :modifiers mods)
                   (ui window)))
    (:repeat
     (alloy:handle (make-instance 'alloy:key-down :code code :key key :modifiers mods :repeat-p T)
                   (ui window)))
    (:release
     (alloy:handle (make-instance 'alloy:key-up :code code :key key :modifiers mods)
                   (ui window)))))

(defmethod glfw:mouse-scrolled ((window window) x y)
  (alloy:handle (make-instance 'alloy:scroll
                                           :dy y
                                           :dx x
                                           :location (cursor-location window))
                (ui window)))

(defmethod glfw:char-entered ((window window) char)
  (alloy:handle (make-instance 'alloy:text-event :text (string (code-char char)))
                (ui window)))

(defun launch ()
  (glfw:init)
  (let ((window (make-instance 'window :opengl-profile :opengl-core-profile
                                       :context-version-major 3
                                       :context-version-minor 3))
        (last-time (glfw:timestamp))
        (resolution (/ 1.0d0 (glfw:timestamp-resolution))))
    (declare (type (unsigned-byte 64) last-time))
    ;; Loop
    (unwind-protect
         (loop until (glfw:should-close-p window)
               do (glfw:poll-events)
                  (let* ((new-time (glfw:timestamp))
                         (dt (* (- new-time last-time) resolution)))
                    (declare (type (unsigned-byte 64) new-time))
                    (animation:update (ui window) (float dt 0f0))
                    (setf last-time new-time))
                  ;; Render UI
                  (gl:viewport 0 0 (glfw:width window) (glfw:height window))
                  (alloy:render window (ui window))
                  (glfw:swap-buffers window))
      (glfw:shutdown))))
