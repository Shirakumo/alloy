#|
 This file is a part of Alloy
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.alloy)

(define-condition alloy-condition (condition) ())

(define-condition no-associated-element (error alloy-condition)
  ((component :initarg :component :reader component)
   (container :initarg :container :reader container))
  (:report (lambda (c s) (format s "The component~%  ~a~%is not associated with any element in~%  ~a"
                                 (component c) (container c)))))

;; TODO: this
