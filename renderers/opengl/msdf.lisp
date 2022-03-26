#|
 This file is a part of Alloy
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage #:org.shirakumo.alloy.renderers.opengl.msdf
  (:use #:cl)
  (:local-nicknames
   (#:alloy #:org.shirakumo.alloy)
   (#:simple #:org.shirakumo.alloy.renderers.simple)
   (#:presentations #:org.shirakumo.alloy.renderers.simple.presentations)
   (#:opengl #:org.shirakumo.alloy.renderers.opengl)
   (#:font-discovery #:org.shirakumo.font-discovery)
   (#:colored #:org.shirakumo.alloy.colored)
   (#:colors #:org.shirakumo.alloy.colored.colors)
   (#:uax-14 #:org.shirakumo.alloy.uax-14))
  (:export
   #:fontcache-default-directory
   #:renderer
   #:fontcache-directory
   #:font
   #:file
   #:cached-font
   #:fontcache
   #:atlas
   #:text
   #:cursor
   #:selection))
(in-package #:org.shirakumo.alloy.renderers.opengl.msdf)

(defvar +default-char+
  '(:xoffset 0 :yoffset 0 :x 0 :y 0 :width 0 :height 0 :xadvance 0))

(defun fontcache-default-directory ()
  ;; KLUDGE: Would be better to use the LOCALAPPDATA envvar, but that requires portable getenv.
  #+win32 (merge-pathnames "AppData/Local/alloy/msdf/" (user-homedir-pathname))
  #-win32 (merge-pathnames ".cache/alloy/msdf/" (user-homedir-pathname)))

(defun fontcache-file (path cache)
  (cond ((or (string= (pathname-type path) "font")
             (string= (pathname-type path) "fnt")
             (string= (pathname-type path) "json"))
         path)
        ((or (string= (pathname-type path) "otf")
             (string= (pathname-type path) "ttf"))
         ;; KLUDGE: would be better to name the files by font properties to collate duplicate files
         ;;         and to avoid pathname length or depth overflow.
         (merge-pathnames (make-pathname :type "json"
                                         :directory (list :relative)
                                         :defaults path)
                          cache))
        (T
         (error "Unrecognised font format for file~%  ~a" path))))

(defclass renderer (opengl:renderer)
  ((fontcache-directory :initarg :fontcache :initform (fontcache-default-directory) :accessor fontcache-directory)
   (fontcache :initform (make-hash-table :test 'equal) :accessor fontcache)
   (font-name-cache :initform (make-hash-table :test 'equalp) :accessor font-name-cache)))

(defmethod initialize-instance :after ((renderer renderer) &key)
  ;; Init font cache
  (dolist (path (directory (make-pathname :name :wild :type "json" :defaults (fontcache-directory renderer))))
    (setf (gethash (pathname-name path) (fontcache renderer))
          (make-instance 'font :family (pathname-name path) :file path :renderer renderer))
    (setf (gethash (pathname-name path) (font-name-cache renderer))
          path)))

(defmethod compute-fallback-chain ((renderer renderer))
  (let ((chain (make-array (hash-table-count (fontcache renderer)))))
    (loop for font being the hash-values of (fontcache renderer)
          for i from 0
          do (setf (aref chain i) font))
    chain))

(defmethod alloy:allocate :before ((renderer renderer))
  (unless (opengl:resource 'text-vbo renderer NIL)
    ;; KLUDGE: pre-allocate a pretty big array to sidestep a driver issue on Windows/nVidia/OpenGL causing segfaults on repeat size changes.
    (setf (opengl:resource 'text-vbo renderer)
          (opengl:make-vertex-buffer renderer (* 10 1024 24)
                                     :data-usage :dynamic-draw))
    (setf (opengl:resource 'text-vao renderer)
          (opengl:make-vertex-array renderer `((,(opengl:resource 'text-vbo renderer) :size 2 :stride 40 :offset 0)
                                               (,(opengl:resource 'text-vbo renderer) :size 2 :stride 40 :offset 8)
                                               (,(opengl:resource 'text-vbo renderer) :size 4 :stride 40 :offset 16)
                                               (,(opengl:resource 'text-vbo renderer) :size 2 :stride 40 :offset 32))))
    (setf (opengl:resource 'text-shader renderer)
          (opengl:make-shader renderer :vertex-shader "
layout (location=0) in vec2 pos;
layout (location=1) in vec2 in_uv;
layout (location=2) in vec4 in_vert_color;
layout (location=3) in vec2 offset;
uniform mat3 transform;
out vec2 uv;
out vec4 vert_color;

void main(){
  gl_Position = vec4(transform*vec3(pos+offset, 1.0), 1.0);
  uv = in_uv;
  vert_color = in_vert_color;
}"
                                       :fragment-shader "
in vec2 uv;
in vec4 vert_color;
uniform sampler2D image;
uniform float pxRange = 32.0;
uniform vec4 color = vec4(0, 0, 0, 1);
out vec4 out_color;

float median(float r, float g, float b){
  return max(min(r, g), min(max(r, g), b));
}

vec2 safeNormalize(in vec2 v){
  float len = length(v);
  len = (len > 0.0)? 1.0 / len : 0.0;
  return v * len;
}

void main(){
vec4 sample = texture(image, uv);
float sigDist = median( sample.r, sample.g, sample.b ) - 0.5;

ivec2 sz = textureSize( image, 0 );
float dx = dFdx( uv.x ) * sz.x;
float dy = dFdy( uv.y ) * sz.y;
float toPixels = pxRange * inversesqrt( dx * dx + dy * dy );
float opacity = clamp( sigDist * toPixels + 0.5, 0.0, 1.0 );

  vec4 frag_col = mix(color, vert_color, vert_color.a);
  out_color = vec4(frag_col.rgb, frag_col.a*opacity);
}"))))

(defmethod alloy:deallocate ((renderer renderer))
  (loop for font being the hash-values of (fontcache renderer)
        do (alloy:deallocate font)))

(defun cached-font (renderer file &optional family)
  (let* ((cache-file (if family
                         (make-pathname :name family :type "json" :defaults (fontcache-directory renderer))
                         (fontcache-file file (fontcache-directory renderer))))
         (cache (gethash cache-file (fontcache renderer))))
    (cond (cache
           cache)
          (T
           (unless (probe-file cache-file)
             (unless (probe-file file)
               (error "Specified font file does not exist or is not accessible.~%  ~a" file))
             (ensure-directories-exist cache-file)
             (sdf-bmfont:create-bmfont file cache-file :size 32 :type :msdf+a))
           (setf (gethash cache-file (fontcache renderer))
                 (make-instance 'font :family family :file cache-file :renderer renderer))))))

(defclass font (simple:font)
  ((renderer :initarg :renderer :accessor renderer)
   (fallback-chain :accessor fallback-chain)
   (file :initarg :file :accessor file)
   (px-range :initform 4.0 :accessor px-range)
   (data :accessor data)
   (atlas :accessor atlas)))

(defmethod compute-fallback-chain ((font font))
  (let* ((chain (compute-fallback-chain (renderer font)))
         (pos (position font chain)))
    (rotatef (aref chain 0) (aref chain pos))
    chain))

(defmethod alloy:allocate ((font font))
  (let ((renderer (renderer font)))
    (unless (slot-boundp font 'data)
      (with-open-file (stream (file font) :external-format :utf-8)
        (setf (data font) (3b-bmfont-json:read-bmfont-json stream)))
      (setf (px-range font) (float (getf (3b-bmfont:distance-field (data font)) :distance-range) 0f0)))
    (unless (slot-boundp font 'atlas)
      (let ((file (merge-pathnames (getf (aref (3b-bmfont:pages (data font)) 0) :file) (file font))))
        (setf (atlas font) (simple:request-image renderer file :filtering :linear))))
    (unless (slot-boundp font 'fallback-chain)
      (flet ((ensure-font (font-ish)
               (typecase font-ish
                 (font font-ish)
                 (T (simple:request-font (renderer font) font-ish)))))
        (setf (fallback-chain font) (map 'vector #'ensure-font (compute-fallback-chain font)))))))

(defmethod alloy:allocated-p ((font font))
  (slot-boundp font 'atlas))

(defmethod alloy:deallocate ((font font))
  (slot-makunbound font 'atlas))

(defmethod simple:request-font ((renderer renderer) (family string) &rest args)
  (let ((font (or (gethash family (font-name-cache renderer))
                  (setf (gethash family (font-name-cache renderer))
                        (font-discovery:file (or (apply #'font-discovery:find-font :family family args)
                                                 (error "No font named ~s found." family)))))))
    (cached-font renderer font family)))

(defmethod simple:request-font ((renderer renderer) (file pathname) &key)
  (cached-font renderer file))

(defmethod simple:request-font ((renderer renderer) (default (eql :default)) &key)
  (simple:request-font renderer "Arial"))

(defmethod simple:request-font ((renderer renderer) (font font) &key)
  font)

(defun compute-glyph-font-sequence (string font-fallback-chain &key start end)
  (let ((result (make-array 0 :adjustable T :fill-pointer T))
        (previous NIL)
        (end (or end (length string))))
    (declare (type string string))
    (declare (type simple-vector font-fallback-chain))
    (loop for i from (or start 0) below end
          for c = (aref string i)
          do (flet ((try-add (font)
                      (unless (eq font previous)
                        (vector-push-extend (cons i font) result)
                        (setf previous font))))
               (unless (eql c #\Space) ;; Special provision for space to avoid frequent switching.
                 (loop for font across font-fallback-chain
                       do (unless (alloy:allocated-p font)
                            (alloy:allocate font))
                          (when (gethash c (3b-bmfont::chars (data font)))
                            (try-add font)
                            (return))
                       finally (try-add (aref font-fallback-chain 0))))))
    (vector-push-extend (cons end (aref font-fallback-chain 0)) result)
    result))

(defun map-glyphs-line (font-sequence function string &key start end)
  (let (w h line space chars kernings
          (%k (cons NIL NIL)))
    (flet ((select-font (font)
             (let ((font (data font)))
               (setf w (float (3b-bmfont::scale-w font)))
               (setf h (float (3b-bmfont::scale-h font)))
               (setf line (3b-bmfont::line-height font))
               (setf space (3b-bmfont::space-size font))
               (setf chars (3b-bmfont::chars font))
               (setf kernings (3b-bmfont::kernings font))))
           (kerning (previous next)
             (setf (car %k) previous)
             (setf (cdr %k) next)
             (gethash %k kernings 0)))
      (loop with y = 0.0
            with x = 0.0
            with si = 0
            for p = nil then c
            for i from (or start 0) below (or end (length string))
            for c = (aref string i)
            do (loop while (<= (car (aref font-sequence si)) i)
                     do (select-font (cdr (aref font-sequence si)))
                        (incf si))
               (let ((char (gethash c chars +default-char+))
                     (k (kerning p c)))
                 (case c
                   (#\newline)
                   (#\space
                    (incf x space))
                   (t
                    (incf x k)
                    (let ((x- (+ x (getf char :xoffset)))
                          (y- (+ y (getf char :yoffset)))
                          (x+ (+ x (getf char :xoffset) (getf char :width)))
                          (y+ (+ y (getf char :yoffset) (getf char :height)))
                          (u- (/ (getf char :x) w))
                          (v- (/ (getf char :y) h))
                          (u+ (/ (+ (getf char :x) (getf char :width)) w))
                          (v+ (/ (+ (getf char :y) (getf char :height)) h)))
                      (psetf y- (- line y+)
                             y+ (- line y-))
                      (funcall function i x- y- x+ y+ u- v- u+ v+))
                    (incf x (getf char :xadvance)))))))))

(defun map-glyphs (font-sequence function string extent scale wrap)
  (declare (type string string))
  (declare (type function function))
  (let ((x 0) (y 0)
        (breaker (uax-14:make-breaker string))
        (max-width (/ (alloy:pxw extent) scale))
        (breaks (make-array 0 :adjustable T :fill-pointer T :element-type '(unsigned-byte 32)))
        (line-start 0)
        (next-break 0)
        (next-mandatory NIL)
        (last-break 0)
        line space chars kernings
        (%k (cons NIL NIL))
        (i 0) (si 0))
    (labels ((select-font (font)
               (let ((font (data font)))
                 (setf line (3b-bmfont::line-height font))
                 (setf space (3b-bmfont::space-size font))
                 (setf chars (3b-bmfont::chars font))
                 (setf kernings (3b-bmfont::kernings font))
                 ;(setf s (/ base-scale (3b-bmfont:base font)))
                 ))
             (kerning (previous next)
               (setf (car %k) previous)
               (setf (cdr %k) next)
               (gethash %k kernings 0))
             (thunk (i x- y- x+ y+ u- v- u+ v+)
               (funcall function i (* scale x-) (* scale (+ y y-)) (* scale x+) (* scale (+ y y+)) u- v- u+ v+))
             (insert-break (at)
               (vector-push-extend at breaks)
               (map-glyphs-line font-sequence #'thunk string :start line-start :end at)
               (setf x 0)
               (decf y line)
               (setf line-start at)))
      ;; This first loops through, only computing the width. Then, when a line break
      ;; is encountered or the line is full, it calls 3b-bmfont:map-glyphs to emit
      ;; the complete line before starting on the next line.
      ;;
      ;; This is obviously stupid, and could be improved a lot by backtracking instead
      ;; when line breaks are needed. But I'm lazy.
      ;;
      ;; FIXME: This model inherently assumes a LTR system and will not deal with
      ;;        RTL, TTB, BTT orientations correctly. We also don't use UAX-9 yet.
      (when (and wrap (< 0 (length string)))
        (loop for p = nil then c
              for c = (aref string i)
              do (loop while (<= (car (aref font-sequence si)) i)
                       do (select-font (cdr (aref font-sequence si)))
                          (incf si))
                 (when (and next-mandatory (= next-break i))
                   (insert-break i))
                 (let ((char (gethash c chars +default-char+))
                       (k (kerning p c)))
                   (case c
                     (#\newline)
                     (#\space
                      (incf x space))
                     (#\tab
                      (incf x (* 8 space)))
                     (t
                      (incf x k)
                      (let ((x+ (+ x (getf char :xoffset) (getf char :width))))
                        (cond ((< x+ max-width))
                              ((< line-start last-break)
                               (insert-break last-break)
                               (setf i last-break)
                               (loop until (< (car (aref font-sequence si)) i)
                                     do (decf si)))
                              ((< line-start i)
                               (insert-break (1- i)))))
                      (incf x (getf char :xadvance))))
                   (when (<= next-break i)
                     (multiple-value-bind (pos mandatory) (uax-14:next-break breaker)
                       (setf next-mandatory mandatory)
                       (shiftf last-break next-break (or pos (1+ (length string))))))
                   (incf i))
              while (< i (length string))))
      (vector-push-extend (length string) breaks)
      (map-glyphs-line font-sequence #'thunk string :start line-start :end (length string))
      breaks)))

(defun compute-text (font text extent scale wrap markup)
  (declare (optimize speed))
  (declare (type string text))
  (let ((array (make-array (* 6 10 (the (signed-byte 32) (length text))) :element-type 'single-float))
        (minx 0.0) (miny 0.0) (maxx 0.0) (maxy 0.0) (i 0)
        (base (3b-bmfont:base (data font)))
        (font-sequence (compute-glyph-font-sequence text (fallback-chain font)))
        (styles ()))
    (declare (type (unsigned-byte 32) i))
    (declare (type single-float minx miny maxx maxy scale))
    (labels ((prop (prop styles)
               (loop for style in styles
                     do (when (eq (car style) prop)
                          (return (rest style)))))
             (vertex (x y u v c)
               (setf (aref array (+ i 0)) (float x))
               (setf (aref array (+ i 1)) (float y))
               (setf (aref array (+ i 2)) (float u))
               (setf (aref array (+ i 3)) (float v))
               (setf (aref array (+ i 4)) (colored:r c))
               (setf (aref array (+ i 5)) (colored:g c))
               (setf (aref array (+ i 6)) (colored:b c))
               (setf (aref array (+ i 7)) (colored:a c))
               (setf (aref array (+ i 8)) 0.0)
               (setf (aref array (+ i 9)) 0.0)
               (incf i 10))
             (thunk (c x- y- x+ y+ u- v- u+ v+)
               (declare (type single-float x- y- x+ y+ u- v- u+ v+))
               (declare (type (unsigned-byte 32) c))
               (loop while (<= (the (unsigned-byte 32) (or (caar markup) #xFFFFFFFF)) c)
                     do (setf styles (cdr (pop markup))))
               (setf minx (min minx x-))
               (setf miny (min miny y-))
               (setf maxx (max maxx x+))
               (setf maxy (max maxy y+))
               (let ((tx (if (prop :italic styles) (* (/ (- y+ y-) base) 15.0) 0.0))
                     (off (if (prop :bold styles) (* scale 5.0) 0.0))
                     (color NIL))
                 (let ((prop (prop :color styles)))
                   (when prop
                     (setf color (first prop))))
                 (let ((prop (prop :rainbow styles)))
                   (when prop
                     (setf color (colored:convert (colored:hsv (* 10 c) 1 1) 'colored:rgb))))
                 (unless color (setf color (colored:rgb 0 0 0 0)))
                 (vertex (- (+ tx x-) off) (+ y+ off) u- (- 1 v-) color)
                 (vertex (- x- off) (- y- off) u- (- 1 v+) color)
                 (vertex (+ tx x+ off) (+ y+ off) u+ (- 1 v-) color)
                 (vertex (+ tx x+ off) (+ y+ off) u+ (- 1 v-) color)
                 (vertex (- x- off) (- y- off) u- (- 1 v+) color)
                 (vertex (+ x+ off) (- y- off) u+ (- 1 v+) color))))
      (values (map-glyphs font-sequence #'thunk text extent scale wrap)
              array font-sequence minx miny maxx maxy))))

(defclass text (simple:text)
  ((vertex-data :accessor vertex-data)
   (font-sequence :initform (make-array 1) :accessor font-sequence)
   (dimensions :accessor dimensions)
   (line-breaks :accessor line-breaks)
   (vertex-count :initform NIL :accessor vertex-count)
   (markup :initform () :accessor markup)
   (clock :initform 0f0 :accessor clock)))

(defmethod org.shirakumo.alloy.animation:update :after ((text text) dt)
  (let* ((markup (markup text))
         (data (vertex-data text))
         (style ())
         (clock (clock text))
         (string (alloy:text text)))
    (when markup
      (incf clock dt)
      (setf (clock text) (mod clock 36.0))
      (flet ((prop (prop style)
               (loop for thing in style
                     do (when (eq prop (car thing))
                          (return (rest thing))))))
        (loop with i = 0
              for c from 0 below (length string)
              do (when (<= (or (caar markup) (length data)) c)
                   (setf style (pop markup)))
                 (when (prop :rainbow (rest style))
                   (let ((color (colored:convert (colored:hsv (mod (+ (* 10 c) (* 200 clock)) 360.0) 1 1) 'colored:rgb)))
                     (loop for j from 0 below 6
                           do (setf (aref data (+ 4 (* 10 (+ j (* 6 i))))) (colored:r color))
                              (setf (aref data (+ 5 (* 10 (+ j (* 6 i))))) (colored:g color))
                              (setf (aref data (+ 6 (* 10 (+ j (* 6 i))))) (colored:b color))
                              (setf (aref data (+ 7 (* 10 (+ j (* 6 i))))) 1.0))))
                 (when (prop :wave (rest style))
                   (let* ((s (scale text))
                          (off (* s 10 (sin (+ (* 0.5 c) (* 10 clock))))))
                     (loop for j from 0 below 6
                           do (setf (aref data (+ 9 (* 10 (+ j (* 6 i))))) off))))
                 (when (prop :shake (rest style))
                   (let* ((s (* 0.02 (scale text)))
                          (time (mod (floor (* clock 50)) 1000))
                          (xoff (* s (logand #xFF (sxhash (+ (* 97 c) time)))))
                          (yoff (* s (logand #xFF (sxhash (+ (* 11 c) time))))))
                     (loop for j from 0 below 6
                           do (setf (aref data (+ 8 (* 10 (+ j (* 6 i))))) xoff)
                              (setf (aref data (+ 9 (* 10 (+ j (* 6 i))))) yoff))))
                 (unless (find (char string c) '(#\Linefeed #\Tab #\Space))
                   (incf i)))))))

(defmethod scale ((text text))
  (/ (alloy:to-px (simple:size text)) (3b-bmfont:base (data (simple:font text)))))

(defmethod shared-initialize :after ((text text) slots &key)
  (alloy:allocate (simple:font text))
  (multiple-value-bind (bounds array font-sequence breaks markup) (alloy:suggest-bounds (alloy:ensure-extent (simple:bounds text)) text)
    (setf (vertex-data text) array)
    (setf (dimensions text) bounds)
    (setf (line-breaks text) breaks)
    (setf (markup text) markup)
    (setf (font-sequence text) font-sequence)))

(defmethod simple:text ((renderer renderer) bounds string &rest args &key font)
  (apply #'make-instance 'text :text string :bounds bounds :font font args))

(defmethod alloy:render ((renderer renderer) (shape text))
  (let* ((shader (opengl:resource 'text-shader renderer))
         (vbo (opengl:resource 'text-vbo renderer))
         (vao (opengl:resource 'text-vao renderer))
         (data (vertex-data shape))
         (count (or (vertex-count shape)
                    (length data))))
    (simple:with-pushed-transforms (renderer)
      (opengl:bind shader)
      (simple:translate renderer (dimensions shape))
      (setf (opengl:uniform shader "transform") (simple:transform-matrix renderer))
      (setf (opengl:uniform shader "color") (simple:pattern shape))
      ;; FIXME: this seems expensive, but maybe it would be worse to statically allocate for each text...
      (opengl:update-vertex-buffer vbo data)
      (let ((sequence (font-sequence shape)))
        (loop for i from 0 below (1- (length sequence))
              for (start . font) = (aref sequence i)
              do (let ((start (* 6 start))
                       (end (min (if (< (1+ i) (length sequence))
                                     (* 6 (car (aref sequence (1+ i))))
                                     count)
                                 count)))
                   (setf (opengl:uniform shader "pxRange") (px-range font))
                   (opengl:bind (atlas font))
                   (opengl:draw-vertex-array vao :triangles start (- end start))
                   (when (<= count end)
                     (return))))))))

(defmethod alloy:suggest-bounds (extent (text text))
  (let ((scale (scale text))
        (markup (simple::flatten-markup (simple:markup text))))
    (multiple-value-bind (breaks array font-sequence x- y- x+ y+) (compute-text (simple:font text) (alloy:text text) extent scale (simple:wrap text) markup)
      (let* ((w (- x+ x-))
             (h (- y+ y-))
             (line (* scale (+ (3b-bmfont:line-height (data (simple:font text))))))
             (base (* scale (3b-bmfont:base (data (simple:font text)))))
             (p (simple:resolve-alignment (simple:bounds text) (simple:halign text) (simple:valign text)
                                          (alloy:px-size w h))))
        (values (alloy:px-extent (alloy:pxx p) (+ (- h line) (alloy:pxy p)) w (if (<= (length breaks) 1) h (+ h base)))
                array font-sequence breaks markup)))))

(defmethod alloy:ideal-bounds ((text text))
  (dimensions text))

(defun estimate-cursor-pos (text point offset)
  (let* ((bounds (dimensions text))
         (font (data (simple:font text)))
         (line-height (3b-bmfont:line-height font))
         (breaks (line-breaks text))
         (x (/ (- (alloy:pxx point) (+ (alloy:pxx bounds) (alloy:pxx offset))) (scale text)))
         (line (min (length breaks) (max 0 (floor (- (+ (alloy:pxy offset) (alloy:pxy bounds)) (alloy:pxy point)) line-height)))))
    (multiple-value-bind (start end)
        (loop for line-start = 0 then line-end
              for line-end across breaks
              do (when (<= line-start line line-end) (return (values line-start line-end)))
              finally (return (values line-start line-end)))
      (block NIL
        (flet ((thunk (i x- y- x+ y+ u- v- u+ v+)
                 (declare (ignore y- y+ u- v- u+ v+))
                 (when (< x- x x+)
                   (return
                     (if (< (- (alloy:pxx point) x-)
                            (- x+ (alloy:pxx point)))
                         i (1+ i))))))
          (map-glyphs-line (font-sequence text) #'thunk (alloy:text text) :start start :end end))
        (return end)))))

(defclass cursor (simple:cursor) ())

(defmethod simple:cursor ((renderer renderer) (text text) (start integer) &rest initargs)
  (apply #'make-instance 'cursor :text text :start start initargs))

(defun compute-cursor-location (text point)
  (let* ((line (loop for prev = 0 then next
                     for next across (line-breaks text)
                     for i from 0
                     do (when (and (<= prev point)
                                   (< point next))
                          (return i))
                     finally (return (1- (length (line-breaks text))))))
         (font (data (simple:font text)))
         (line-height (3b-bmfont:line-height font))
         (x (3b-bmfont:measure-glyphs font (alloy:text text)
                                      :start (if (= 0 line) 0 (aref (line-breaks text) (1- line)))
                                      :end point))
         (s (scale text))
         (d (dimensions text)))
    ;; KLUDGE: Dunno how I could do this cleanly.
    (when (and (= (length (alloy:text text)) point)
               (< 0 point)
               (char= #\Linefeed (char (alloy:text text) (1- point))))
      (incf line))
    (alloy:px-extent (* s x) (- (alloy:pxy d) (* s line-height line))
                     (* s 4) (* s line-height))))

(defmethod shared-initialize :after ((cursor cursor) slots &key)
  (setf (simple:bounds cursor) (compute-cursor-location (simple:text-object cursor) (simple:start cursor))))

(defmethod alloy:handle :after ((event alloy:pointer-down) (component alloy:text-input-component))
  (let ((label (presentations:find-shape :label component)))
    (when label
      (alloy:with-unit-parent component
        (alloy:move-to (estimate-cursor-pos label (alloy:location event) (alloy:bounds component)) component)))))

(defclass selection (opengl::polygon simple:selection) ())

(defmethod simple:selection ((renderer renderer) (text text) (start integer) (end integer) &rest initargs)
  (apply #'make-instance 'selection :text text :start start :end end initargs))

(defmethod shared-initialize ((selection selection) slots &key)
  (call-next-method)
  (if (/= (simple:start selection) (simple:end selection))
      (let ((start (compute-cursor-location (simple:text-object selection) (simple:start selection)))
            (end (compute-cursor-location (simple:text-object selection) (simple:end selection))))
        (setf (simple:points selection)
              (list (alloy:px-point (alloy:pxx start) (alloy:pxy start))
                    (alloy:px-point (alloy:pxx end) (alloy:pxy end))
                    (alloy:px-point (alloy:pxx end) (+ (alloy:pxy end) (alloy:pxh end)))
                    (alloy:px-point (alloy:pxx start) (+ (alloy:pxy end) (alloy:pxh end))))))
      (setf (simple:points selection) ())))
