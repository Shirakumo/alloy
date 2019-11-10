#|
 This file is a part of Alloy
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.alloy)

(defclass scroll-view (structure)
  ())

(defmethod initialize-instance :after ((structure scroll-view) &key layout focus (scroll T))
  (let ((border-layout (make-instance 'border-layout))
        (focus-list (make-instance 'focus-list))
        (clipper (make-instance 'clip-view)))
    (enter clipper border-layout :position :center)
    (enter layout clipper)
    (when focus
      (enter focus focus-list))
    (when (or (eql scroll T) (eql scroll :y))
      (let ((scrollbar (represent clipper 'y-scrollbar)))
        (enter scrollbar border-layout :position :east)
        (enter scrollbar focus-list)))
    (when (or (eql scroll T) (eql scroll :x))
      (let ((scrollbar (represent clipper 'x-scrollbar)))
        (enter scrollbar border-layout :position :south)
        (enter scrollbar focus-list)))
    (finish-structure structure border-layout focus-list)))
