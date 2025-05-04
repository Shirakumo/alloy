(in-package #:org.shirakumo.alloy.examples)

(defclass styled-label (alloy:input-box)
  ((font :initarg :font :initform "Arial" :accessor font)
   (size :initarg :size :initform 14 :accessor size)
   (halign :initarg :halign :initform :start :accessor halign)
   (valign :initarg :valign :initform :top :accessor valign)))

(defmethod (setf font) :after (font (label styled-label)) (alloy:mark-for-render label))
(defmethod (setf size) :after (font (label styled-label)) (alloy:mark-for-render label))
(defmethod (setf halign) :after (font (label styled-label)) (alloy:mark-for-render label))
(defmethod (setf valign) :after (font (label styled-label)) (alloy:mark-for-render label))

(presentations:define-update (alloy:renderer styled-label)
  (:label
   :font (simple:request-font alloy:renderer (font alloy:renderable))
   :size (alloy:px (size alloy:renderable))
   :halign (halign alloy:renderable)
   :valign (valign alloy:renderable)))

(define-example fonts (screen)
  "Render text using different fonts available on the system."
  (let* ((window (windowing:make-window screen))
         (layout (make-instance 'alloy:grid-layout :col-sizes '(T) :row-sizes '(30 T) :layout-parent window))
         (focus (make-instance 'alloy:focus-list :focus-parent window))
         (text "The quick brown fox jumps over the lazy dog.")
         (label (alloy:represent text 'styled-label :focus-parent focus)))
    (let ((controls (make-instance 'alloy:horizontal-linear-layout :min-size (alloy:size 50 20) :layout-parent layout)))
      (alloy:represent (font label) 'alloy:combo-set
                       :value-set (sort (delete-duplicates
                                         (loop for font in (org.shirakumo.font-discovery:list-fonts)
                                               when (string-equal "ttf" (pathname-type (org.shirakumo.font-discovery:file font)))
                                               collect (org.shirakumo.font-discovery:family font))
                                         :test #'string-equal)
                                        #'string<)
                       :layout-parent controls
                       :focus-parent focus)
      (alloy:represent (size label) 'alloy:ranged-wheel
                       :layout-parent controls
                       :focus-parent focus)
      (alloy:represent (halign label) 'alloy:combo-set
                       :value-set '(:start :middle :end)
                       :layout-parent controls
                       :focus-parent focus)
      (alloy:represent (valign label) 'alloy:combo-set
                       :value-set '(:top :middle :bottom)
                       :layout-parent controls
                       :focus-parent focus))
    (alloy:enter label layout)))
