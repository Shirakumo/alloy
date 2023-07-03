(in-package #:org.shirakumo.alloy)

(defclass printable (input-line validated-text-input transformed-text-input)
  ())

(defmethod valid-p ((printable printable) text)
  (ignore-errors
   (read-from-string text)
   T))

(defmethod value->text ((printable printable) expr)
  (prin1-to-string expr))

(defmethod text->value ((printable printable) text)
  (read-from-string text))
