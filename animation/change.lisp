#|
 This file is a part of Alloy
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.alloy.animation)

(defun compile-trackers (trackers)
  (let ((extractors ())
        (comparators ())
        (tween-infos ()))
    (dolist (tracker trackers (list extractors comparators tween-infos))
      (destructuring-bind (extractor &key (duration 1.0) (easing 'linear) (comparison 'equalp)) tracker
        (push extractor extractors)
        (push comparison comparators)
        (push (list :duration duration :easing easing) tween-infos)))))

(defun compile-tween (extractor old-val new-val info)
  (destructuring-bind (&key duration easing) info
    `(make-tween #'(setf ,extractor)
                 ,(%expand-array (list 0.0 duration) :element-type 'single-float)
                 ,(%expand-array (list old-val new-val))
                 ,(%expand-array (list (easing easing))))))

(defun compile-change-tracker (animated tracked next-method)
  (destructuring-bind (extractors comparators tween-infos) (compile-trackers tracked)
    `(let (,@(loop for extractor in extractors
                   collect `(,extractor (,extractor ,animated)))
           (animations (make-array ,(length extractors) :fill-pointer 0)))
       (prog1 (funcall ,next-method)
         ,@(loop for extractor in extractors
                 for comparator in comparators
                 for tween-info in tween-infos
                 collect `(let ((new (,extractor ,animated)))
                            (unless (,comparator new ,extractor)
                              (vector-push ,(compile-tween extractor extractor 'new tween-info) animations))))
         (when (< 0 (fill-pointer animations))
           (apply-animation animations ,animated)
           (update ,animated 0.0))))))
