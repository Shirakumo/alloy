(in-package #:org.shirakumo.alloy)

(defclass combo-item (button direct-value-component)
  ())

(defclass combo-layout (vertical-linear-layout)
  ((parent :initarg :parent :accessor parent)))

(defmethod handle ((event scroll) (layout combo-layout))
  (let ((extent (bounds layout))
        (pextent (bounds (parent layout))))
    (setf (y layout) (min (max (+ (pxy extent) (* -20 (dy event)))
                               (- (pxh pextent) (pxh extent)))
                          0.0))))

(defmethod notice-size ((layout combo-layout) (parent (eql T)))
  (let ((ideal (suggest-size (bounds (parent layout)) layout)))
    (resize layout (w ideal) (h ideal))))

(defclass combo (value-component focus-list)
  ((state :initform NIL :accessor state)
   (combo-list :reader combo-list)
   (typed :initform "" :accessor typed)
   (last-typed :initform 0 :accessor last-typed)))

(defmethod initialize-instance ((combo combo) &key)
  (call-next-method)
  (setf (slot-value combo 'combo-list) (make-instance 'combo-layout :cell-margins (margins) :parent combo))
  (setf (slot-value (slot-value combo 'combo-list) 'layout-parent) combo))

(defgeneric combo-item (item combo))
(defgeneric value-set (data))
(define-observable (setf value-set) (set observable))

(defmethod notice-size (thing (combo combo)))

(defmethod value-changed :after ((combo combo))
  ;; FIXME: This is bad
  (do-elements (element combo)
    (when (eql (value combo) (value element))
      (return (setf (focused combo) element)))))

(defmethod initialize-instance :after ((combo combo) &key)
  (update-combo-items combo (value-set combo))
  (on value-set (set (data combo))
    (update-combo-items combo set)))

(defmethod set-layout-tree :before (value (combo combo))
  (set-layout-tree value (combo-list combo)))

(defmethod (setf index) :after (index (combo combo))
  (when (focused combo)
    (let ((ib (bounds (focused combo))))
      (setf (y (combo-list combo)) (- (pxy ib)))))
  (mark-for-render combo))

(defmethod (setf focused) :after (index (combo combo))
  (let ((ib (bounds (focused combo))))
    (setf (y (combo-list combo)) (- (pxy ib))))
  (mark-for-render combo))

(defmethod text ((combo combo))
  (if (focused combo)
      (text (focused combo))
      (princ-to-string (value combo))))

(defmethod notice-size ((list combo-layout) (combo combo))
  (notice-size combo T))

(defmethod activate :after ((combo combo))
  (setf (state combo) :selecting)
  (refit combo)
  (setf (index combo) (index combo)))

(defmethod handle ((event text-event) (combo combo))
  (let ((typed (concatenate 'string
                            (if (< internal-time-units-per-second (- (get-internal-real-time) (last-typed combo)))
                                ""
                                (typed combo))
                            (text event))))
    (setf (last-typed combo) (get-internal-real-time))
    (setf (typed combo) typed)
    (do-elements (element combo)
      (let ((text (text element)))
        (when (and (<= (length typed) (length text))
                   (string-equal typed text :end2 (length typed)))
          (setf (focused combo) element)
          (return))))))

(defmethod handle ((event key-down) (combo combo))
  (when (and (< 0 (element-count combo))
             (eql (state combo) :selecting))
    (flet ((set-index (index)
             (setf (index combo) (max 0 (min (1- (element-count combo)) index)))
             (setf (value combo) (value (focused combo)))))
      (case (key event)
        (:page-up
         (set-index (- (index combo) 10)))
        (:page-down
         (set-index (+ (index combo) 10)))
        (:home
         (set-index 0))
        (:end
         (set-index (1- (element-count combo))))
        (:enter
         (when (focused combo)
           (setf (value combo) (value (focused combo)))
           (setf (state combo) NIL)))
        (T (call-next-method))))))

(defmethod handle :around ((event pointer-event) (combo combo))
  (case (state combo)
    (:selecting
     (unless (handle event (combo-list combo))
       (when (typep event 'pointer-up)
         (exit combo)))
     T)
    (T
     (call-next-method))))

(defmethod handle ((event scroll) (combo combo))
  (setf (index combo) (max 0 (min (1- (element-count combo)) (+ (or (index combo) 0) (if (< 0 (dy event)) -1 +1)))))
  (setf (value combo) (value (focused combo))))

(defmethod (setf focus) :after (focus (combo combo))
  (when (null focus)
    (setf (state combo) NIL)))

(defmethod exit :after ((combo combo))
  (setf (value combo) (value combo))
  (setf (typed combo) ""))

(defmethod combo-item (item (combo combo))
  (make-instance 'combo-item :value item))

(defmethod update-combo-items ((combo combo) items)
  (let ((list (combo-list combo)))
    ;; TODO: It may be possible to optimise this to only insert and remove
    ;;       items as necessary, but ensuring the order is as specified in
    ;;       the items list seems difficult to do without sacrificing efficiency.
    (clear list)
    (clear combo)
    (flet ((add (el)
             (let ((item (combo-item el combo)))
               (enter item list)
               (enter item combo)
               (on activate (item)
                 (setf (value combo) (value item))
                 (setf (state combo) NIL))
               (when (eql el (value combo))
                 (setf (focused combo) item)))))
      (etypecase items
        (list (loop for item in items do (add item)))
        (vector (loop for item across items do (add item)))))))

(defmethod suggest-size (size (combo combo))
  (suggest-size size (combo-list combo)))

(defmethod register :after ((combo combo) (renderer renderer))
  (register (combo-list combo) renderer))

(defmethod deregister :after ((combo combo) (renderer renderer))
  (deregister (combo-list combo) renderer))

(defmethod render :after ((renderer renderer) (combo combo))
  (case (state combo)
    (:selecting
     (reset-visibility renderer)
     (render renderer (combo-list combo)))))

(defmethod (setf bounds) :after (bounds (combo combo))
  (refit combo))

(defmethod refit ((combo combo))
  (let* ((bounds (bounds combo))
         (ideal (suggest-size bounds (combo-list combo))))
    (setf (bounds (combo-list combo))
          (px-extent 0
                     (- (pxh bounds) (pxh ideal))
                     (pxw ideal)
                     (pxh ideal)))))

(defclass combo-set (combo)
  ((value-set :initform (arg! :value-set) :initarg :value-set :accessor value-set)))

(defmethod (setf value-set) :after (set (combo combo-set))
  (update-combo-items combo set))
