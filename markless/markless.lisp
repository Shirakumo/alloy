(in-package #:org.shirakumo.alloy.markless)

(defclass document-layout (alloy:vertical-linear-layout)
  ((alloy:cell-margins :initform (alloy:margins))))

(defclass document-element (alloy:layout-element)
  ((margins :initform (alloy:margins) :accessor margins)
   (padding :initform (alloy:margins) :accessor padding)))

(defclass document-text (alloy:direct-value-component)
  ((alloy:value :accessor alloy:text)
   (markup :initarg :markup :initform NIL :accessor markup)))

(defclass document (document-layout)
  ())

(defclass paragraph (document-text document-element)
  ())

(defclass blockquote-header (document-text document-element)
  ())

(defclass blockquote (document-layout document-element)
  ())

(defclass ordered-list (document-layout document-element)
  ())

(defclass ordered-list-item (document-layout document-element alloy:renderable)
  ((number :initarg :number :initform 0 :accessor number)))

(defclass unordered-list (document-layout document-element)
  ())

(defclass unordered-list-item (document-layout document-element alloy:renderable)
  ())

(defclass header (document-text document-element)
  ((depth :initarg :depth :initform 0 :accessor depth)))

(defclass horizontal-rule (document-element alloy:renderable)
  ())

(defclass code-block (document-text document-element)
  ((language :initarg :language :initform NIL :accessor language)
   (options :initarg :options :initform () :accessor options)))

(defclass embed (document-element)
  ((caption :initarg :caption :initform NIL :accessor caption)
   (width :initarg :width :initform NIL :accessor width)
   (height :initarg :height :initform NIL :accessor height)
   (link :initarg :link :initform NIL :accessor link)))

(defmethod alloy:activate ((element embed))
  (when (link element)
    ;; FIXME: Open URL.
    ))

(defclass image (alloy:icon embed)
  ())

(defclass footnote (document-layout document-element)
  ((number :initarg :number :initform 0 :accessor number)))

(defgeneric translate (component container))
