(in-package #:org.shirakumo.alloy.animation)

(defvar *animations* (make-hash-table :test 'eql))

(defun animation (name &optional (errorp T))
  (or (gethash name *animations*)
      (when errorp (error "No animation named ~s" name))))

(defun (setf animation) (tweens name)
  (if tweens
      (setf (gethash name *animations*) tweens)
      (remhash name *animations*))
  tweens)

(defun list-animations ()
  (loop for name being the hash-keys of *animations* collect name))

(defgeneric update (animation dt))
(defgeneric apply-animation (animation animated))

;; TODO: fold tween into animation
(defstruct (tween (:constructor make-tween (setter stops values easings &optional (loop NIL))))
  (setter NIL :type function)
  (stops NIL :type (simple-array single-float (*)))
  (values NIL :type simple-vector)
  (easings NIL :type simple-vector)
  (idx 0 :type (unsigned-byte 32))
  (clock 0f0 :type single-float)
  (loop NIL :type T))

(defun find-tween-idx (tween clock)
  ;; TODO: could speed this up with bin search, but not sure if worth it.
  ;;       Presumably there's not going to be a lot of stops to make it worth it.
  (let ((stops (tween-stops tween)))
    (loop for i from 1 below (length stops)
          do (when (< clock (aref stops i))
               (return (1- i)))
          finally (return (length stops)))))

(defclass animated ()
  ((tweens :initform #() :accessor tweens)))

(defmethod shared-initialize :after ((animated animated) slots &key (tweens NIL tweens-p))
  (when tweens-p
    (let ((old (tweens animated)))
      (loop for i from 0 below (length tweens)
            for tween = (aref tweens i)
            for setter = (tween-setter tween)
            for clock = (loop for tween across old
                              do (when (eql setter (tween-setter tween))
                                   (return (tween-clock tween)))
                              finally (return 0f0))
            do (setf (tween-idx tween) (find-tween-idx tween clock))
               (setf (tween-clock tween) clock))
      (setf (tweens animated) tweens))))

(defmethod update ((animated animated) dt)
  (declare (type single-float dt))
  (loop for tween across (tweens animated)
        for setter = (tween-setter tween)
        for stops = (tween-stops tween)
        for values = (tween-values tween)
        for easings = (tween-easings tween)
        for idx = (tween-idx tween)
        for clock = (tween-clock tween)
        for max = (1- (length stops))
        do (when (<= idx max)
             (incf clock dt)
             (setf (tween-clock tween) clock)
             (let ((max-time (aref stops (min max (1+ idx)))))
               (when (< max-time clock)
                 (incf idx)
                 (setf (tween-idx tween) idx))
               (cond ((< idx max)
                      (let* ((ai idx)
                             (bi (1+ idx))
                             (a (aref stops ai))
                             (b (aref stops bi))
                             (aval (aref values ai))
                             (bval (aref values bi))
                             (ease (aref easings idx))
                             (x (max 0.0 (/ (- clock a) (- b a))))
                             (val (lerp aval bval (funcall ease x))))
                        (funcall setter val animated)))
                     (T
                      (funcall setter (aref values max) animated)
                      (etypecase (tween-loop tween)
                        ((eql T)
                         (setf clock (- clock max-time)))
                        (real
                         (setf clock (+ (tween-loop tween) (- clock max-time))))
                        (null))
                      (when (tween-loop tween)
                        (setf (tween-clock tween) clock)
                        (setf (tween-idx tween) (loop for i from 0 below max
                                                      do (when (<= (aref stops i) clock)
                                                           (return i))
                                                      finally (return 0)))))))))
  ;; KLUDGE: trim expired tweens
  (setf (tweens animated) (remove-if (lambda (tween) (<= (length (tween-stops tween)) (tween-idx tween)))
                                     (tweens animated))))

(defmethod update ((element alloy:layout-element) dt)
  (when (next-method-p)
    (call-next-method)))

(defmethod update :around ((element alloy:layout-element) dt)
  ;; KLUDGE: animated might be a superclass of layout-element
  (alloy:with-unit-parent element
    (call-next-method)))

(defmethod update ((layout alloy:layout) dt)
  (when (next-method-p)
    (call-next-method))
  (alloy:do-elements (element layout)
    (update element dt)))

(defmethod update ((tree alloy:layout-tree) dt)
  (update (alloy:root tree) dt)
  (update (alloy:popups tree) dt))

(defmethod update ((ui alloy:ui) dt)
  (update (alloy:layout-tree ui) dt))

(defmethod apply-animation ((name symbol) (animated animated))
  (reinitialize-instance animated :tweens (funcall (animation name))))

(defmethod apply-animation ((tweens vector) (animated animated))
  (reinitialize-instance animated :tweens tweens))

(defun %expand-array (values &rest array-options)
  (let ((array (gensym "ARRAY")))
    `(let ((,array (make-array ,(length values) ,@(loop for opt in array-options collect `',opt))))
       ,@(loop for i from 0 below (length values)
               for value in values
               collect `(setf (aref ,array ,i) ,value))
       ,array)))

(defun compile-progression (progression &key loop)
  (let ((tweens (make-hash-table :test 'equal)))
    (check-type (first progression) real)
    ;; Restructure into per-place tween info
    (loop while progression
          do (let ((stop (pop progression))
                   (parts (loop until (or (null progression)
                                          (realp (first progression)))
                                do (check-type (first progression) cons)
                                collect (pop progression))))
               (when parts
                 (dolist (part parts)
                   (destructuring-bind (place value &key (easing 'linear)) part
                     (push (list stop value easing) (gethash place tweens)))))))
    (loop for place being the hash-keys of tweens
          for data being the hash-values of tweens
          do (setf (gethash place tweens) (nreverse data)))
    ;; Compile down to constructors
    (loop for place being the hash-keys of tweens
          for data being the hash-values of tweens
          for stops = (loop for (stop) in data collect (float stop 0f0))
          for values = (loop for (_ value) in data collect value)
          for easings = (loop for (_ __ easing) in (rest data) collect `(easing ',easing))
          collect `(make-tween ,(etypecase place
                                  (symbol `(fdefinition ',place))
                                  (cons (case (first place)
                                          (setf `(fdefinition ',place))
                                          (T place))))
                               ,(%expand-array stops :element-type 'single-float)
                               ,(%expand-array values)
                               ,(%expand-array easings)
                               ,loop))))

(defmacro define-animation (name &body progression)
  (destructuring-bind (name &key loop) (if (listp name) name (list name))
    (let ((tweens (compile-progression progression :loop loop)))
      `(setf (animation ',name)
             (lambda ()
               (declare (optimize speed))
               ,(%expand-array tweens))))))
