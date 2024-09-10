(in-package #:org.shirakumo.alloy.animation)

(defun compile-trackers (trackers)
  (let ((extractors ())
        (comparators ())
        (tween-infos ()))
    (dolist (tracker trackers (list extractors comparators tween-infos))
      (destructuring-bind (extractor &key (delay 0.0) (duration 1.0) (easing 'linear) (comparison 'equalp)) tracker
        (push extractor extractors)
        (push comparison comparators)
        (push (list :delay delay :duration duration :easing easing) tween-infos)))))

(defun compile-tween (extractor old-val new-val info)
  (destructuring-bind (&key delay duration easing) info
    `(make-tween #'(setf ,extractor)
                 ,(%expand-array (list 0.0 duration) :element-type 'single-float)
                 ,(%expand-array (list old-val new-val))
                 ,(%expand-array (list `(load-time-value (easing ',easing))))
                 NIL (float ,delay 0f0))))

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

(defclass smooth-scrolling-clip-view (alloy:clip-view)
  ((real-offset :initform (alloy:px-point 0 0) :accessor real-offset)
   (scroll-delay :initarg :scroll-delay :initform 0.2 :accessor scroll-delay)
   (scroll-time :initform 0.0 :accessor scroll-time)))

(defmethod (setf alloy:offset) (value (layout smooth-scrolling-clip-view))
  (unless (alloy:point= value (real-offset layout))
    (setf (scroll-time layout) (scroll-delay layout))
    (setf (real-offset layout) value)))

(defmethod update ((layout smooth-scrolling-clip-view) dt)
  (let ((src (real-offset layout))
        (dst (alloy:offset layout))
        (rem (scroll-time layout)))
    (when (< 0 rem)
      (if (< 0 (setf (scroll-time layout) (max 0.0 (- (scroll-time layout) dt))))
          (flet ((tween (from to)
                   (let* ((l (- (/ (float rem 0f0) (float (log 1/100 2) 0f0))))
                          (n (- 1 (expt 2 (- (/ (float dt 0f0) l))))))
                     (declare (type single-float n l))
                     (lerp from to n))))
            (setf (alloy:unit-value (alloy:x dst))
                  (tween (alloy:unit-value (alloy:x dst)) (alloy:unit-value (alloy:x src))))
            (setf (alloy:unit-value (alloy:y dst))
                  (tween (alloy:unit-value (alloy:y dst)) (alloy:unit-value (alloy:y src)))))
          (setf (alloy:unit-value (alloy:x dst)) (alloy:unit-value (alloy:x src))
                (alloy:unit-value (alloy:y dst)) (alloy:unit-value (alloy:y src))))
      (alloy:notify-observers 'alloy:value layout dst layout))))
