#|
 This file is a part of Alloy
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.alloy)

(defclass searchable (vertical-linear-layout focus-list value-component)
  ())

(defmethod initialize-instance :after ((component searchable) &key data)
  (let ((results (make-instance 'list))))
  (enter (make-instance 'input-line :data data) component)
  (enter (make-instance 'scroll-view :layout results :scroll :y) component))

(defgeneric query (data query))
