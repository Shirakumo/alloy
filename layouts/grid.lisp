(in-package #:org.shirakumo.alloy)

(defclass grid-constraints-mixin ()
  ((cell-margins :initarg :cell-margins :initform (margins 5) :accessor cell-margins)
   (row-sizes :initform (make-array 0 :adjustable T :fill-pointer T) :reader row-sizes)
   (col-sizes :initform (make-array 0 :adjustable T :fill-pointer T) :reader col-sizes)))

(defmethod shared-initialize :after ((layout grid-constraints-mixin) slots
                                     &key (row-sizes NIL r-p) (col-sizes NIL c-p))
  (when r-p (setf (row-sizes layout) row-sizes))
  (when c-p (setf (col-sizes layout) col-sizes)))

(defun coerce-grid-size (v)
  (etypecase v
    (real (un v))
    (unit v)
    ((eql T) T)))

(defmethod (setf row-sizes) ((new-value sequence) (layout grid-constraints-mixin))
  (let ((length (length new-value))
        (value (row-sizes layout)))
    (adjust-array value length :fill-pointer length)
    (map-into value #'coerce-grid-size new-value))
  new-value)

(defmethod (setf col-sizes) ((new-value sequence) (layout grid-constraints-mixin))
  (let ((length (length new-value))
        (value (col-sizes layout)))
    (adjust-array value length :fill-pointer length)
    (map-into value #'coerce-grid-size new-value))
  new-value)

(defclass grid-layout (grid-constraints-mixin layout vector-container)
  ((stretch :initarg :stretch :initform T :accessor stretch)))

(defmethod (setf stretch) :after (value (layout grid-layout))
  (suggest-size (bounds layout) layout))

(defmethod (setf row-sizes) :after ((new-value sequence) (layout grid-layout))
  (adjust-grid layout (length new-value) (length (col-sizes layout))))

(defmethod (setf col-sizes) :after ((new-value sequence) (layout grid-layout))
  (adjust-grid layout (length (row-sizes layout)) (length new-value)))

(defmethod adjust-grid ((layout grid-layout) nrows ncols)
  (let ((orows (length (row-sizes layout)))
        (ocols (length (col-sizes layout)))
        (old (elements layout))
        (new (make-array (* nrows ncols) :initial-element NIL :adjustable T :fill-pointer 0)))
    ;; Leave old ones
    (loop for row from nrows below orows
          do (loop for col from ncols below ocols
                   do (leave (aref old (+ col (* row ocols))) layout)))
    ;; Populate new grid
    (loop for row from 0 below (min nrows orows)
          do (loop for col from 0 below (min ncols ocols)
                   for oidx = (+ col (* row ocols))
                   while (< oidx (fill-pointer old))
                   do (vector-push (aref old oidx) new)))
    ;; Update if possible
    (setf (slot-value layout 'elements) new)
    (when (slot-boundp layout 'layout-parent)
      (suggest-size (bounds layout) layout))))

(defmethod enter :before ((element layout-element) (layout grid-layout) &key row col)
  (when (and row col)
    (let ((idx (+ col (* row (length (col-sizes layout))))))
      (when (and (< idx (length (elements layout)))
                 (aref (elements layout) idx))
        (error 'place-already-occupied
               :bad-element element :place (cons row col) :layout layout :existing (aref (elements layout) idx))))))

(defmethod enter ((element layout-element) (layout grid-layout) &key row col index)
  (let ((index (if (and row col)
                   (+ col (* row (length (col-sizes layout))))
                   (or index (length (elements layout))))))
    (when (and index (<= (length (elements layout)) index))
      (adjust-array (elements layout) (1+ index) :initial-element NIL :fill-pointer (1+ index)))
    (setf (aref (elements layout) index) element))
  ;; Extend rows as much as necessary
  (loop while (< (* (length (row-sizes layout)) (length (col-sizes layout)))
                 (fill-pointer (elements layout)))
        do (vector-push-extend (aref (row-sizes layout) (1- (length (row-sizes layout)))) (row-sizes layout)))
  (reevaluate-grid-size layout))

(defmethod leave ((element layout-element) (layout grid-layout))
  (setf (aref (elements layout) (element-index element layout)) NIL)
  element)

(defmethod update ((element layout-element) (layout grid-layout) &key row col index)
  (when (or index (and row col))
    (let ((idx (element-index element layout)))
      (setf (aref (elements layout) idx) NIL))
    (array-utils:vector-push-extend-position element (elements layout)
                                             (or index (+ col (* row (length (col-sizes layout))))))
    ;; Extend rows as much as necessary
    (loop while (< (* (length (row-sizes layout)) (length (col-sizes layout)))
                   (fill-pointer (elements layout)))
          do (vector-push-extend (aref (row-sizes layout) (1- (length (row-sizes layout)))) (row-sizes layout)))
    (reevaluate-grid-size layout))
  element)

(defmethod call-with-elements (function (layout grid-layout) &rest args)
  (apply #'call-next-method (lambda (el) (when el (funcall function el))) layout args))

(defmethod clear ((layout grid-layout))
  (loop for i downfrom (1- (length (elements layout))) to 0
        for element = (aref (elements layout) i)
        do (when element (leave element layout)))
  (setf (fill-pointer (elements layout)) 0)
  (reevaluate-grid-size layout))

(defmethod refit ((layout grid-layout))
  (with-unit-parent layout
    (destructure-margins (:l ml :u mu :r mr :b mb :to-px T) (cell-margins layout)
      (let* ((extent (bounds layout))
             (th (spanning-size (row-sizes layout) (size-h extent)))
             (tw (spanning-size (col-sizes layout) (size-w extent))))
        (loop with elements = (elements layout)
              for y = (+ (pxh extent) (- mb)) then (- y h)
              for hish across (row-sizes layout)
              for h = (if (eql T hish) th (to-px hish))
              for i from 0
              do (loop for x = ml then (+ x w)
                       for wish across (col-sizes layout)
                       for w = (if (eql T wish) tw (to-px wish))
                       for j from 0
                       for idx from (* i (length (col-sizes layout)))
                       while (< idx (fill-pointer elements))
                       do (let ((element (aref elements idx)))
                            (when element
                              (let ((ideal (suggest-size (px-size (- w ml mr) (- h mb mu)) element)))
                                (if (and (< 0 (pxh ideal)) (< 0 (pxw ideal)))
                                    (setf (bounds element)
                                          (px-extent x (- y (if (stretch layout) (- h mb mu) (pxh ideal)))
                                                     (if (stretch layout) (- w ml mr) (size-w ideal))
                                                     (if (stretch layout) (- h mb mu) (size-h ideal))))
                                    (setf (bounds element) (px-extent 0 0 0 0))))))))))))

(defun spanning-size (sizes total)
  (let ((count 0)
        (total (to-px total)))
    (loop for size across sizes
          do (etypecase size
               (unit (decf total (to-px size)))
               ((eql T) (incf count))))
    (max 0.0 (if (< 0 count)
                 (/ total count)
                 total))))

(defmethod notice-size ((target layout-element) (layout grid-layout))
  (refit layout))

(defmethod (setf bounds) :after (extent (layout grid-layout))
  (refit layout))

(defmethod suggest-size (size (layout grid-layout))
  (with-unit-parent layout
    (destructure-margins (:l ml :u mu :r mr :b mb :to-px T) (cell-margins layout)
      (let* ((cols (col-sizes layout))
             (rows (row-sizes layout))
             (elements (elements layout))
             (col-count (length cols))
             (row-count (length rows))
             (wtotal (loop for col across cols
                           unless (eql T col) sum (to-px col)))
             (htotal (loop for row across rows
                           unless (eql T row) sum (to-px row)))
             (flex-cols (loop for col across cols count (eql T col)))
             (flex-rows (loop for row across rows count (eql T row)))
             (suggested (px-size (- (/ (- (pxw size) wtotal) (max 1 flex-cols)) ml mr)
                                 (- (/ (- (pxh size) htotal) (max 1 flex-rows)) mu mb))))
        (flet ((el (row col)
                 (let ((idx (+ col (* row col-count))))
                   (when (< idx (length elements))
                     (aref elements idx)))))
          (loop for col from 0 below col-count
                do (when (eql (aref cols col) T)
                     (incf wtotal (+ (loop for row from 0 below row-count
                                           for el = (el row col)
                                           maximize (if el (pxw (suggest-size suggested el)) 0.0))
                                     ml mr))))
          (loop for row from 0 below row-count
                do (when (eql (aref rows row) T)
                     (incf htotal (+ (loop for col from 0 below col-count
                                           for el = (el row col)
                                           maximize (if el (pxh (suggest-size suggested el)) 0.0))
                                     mu mb))))
          (px-size wtotal htotal))))))

(defun reevaluate-grid-size (layout)
  (when (layout-tree layout)
    (setf (bounds layout) (suggest-size (bounds layout) layout))
    (notice-size layout T)))
