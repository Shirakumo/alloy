(in-package #:org.shirakumo.alloy)

(defclass linear-layout (layout vector-container)
  (;; Minimal width and height of contained elements.
   (min-size :initarg :min-size :initform (size 20 20) :accessor min-size)
   (stretch :initarg :stretch :initform T :accessor stretch)
   (align :initarg :align :initform :start :accessor align)
   (cell-margins :initarg :cell-margins :initform (margins 2) :accessor cell-margins)))

(defgeneric fit-linear-layout-contents (layout extent))

(defmethod (setf align) :after (value (layout linear-layout))
  (setf (layout-needed-p layout) T))

(defmethod (setf stretch) :after (value (layout linear-layout))
  (setf (layout-needed-p layout) T))

(defmethod (setf min-size) :after (value (layout linear-layout))
  (setf (layout-needed-p layout) T))

(defmethod (setf cell-margins) :after (value (layout linear-layout))
  (setf (layout-needed-p layout) T))

(defmethod leave :after ((element layout-element) (layout linear-layout))
  (setf (layout-needed-p layout) T))

(defmethod notice-size :after ((element layout-element) (layout linear-layout))
  (notice-size layout T))

(defclass vertical-linear-layout (linear-layout)
  ((align :initform :end)))

(defmethod suggest-size (size (layout vertical-linear-layout))
  (with-unit-parent layout
    (destructure-margins (:l l :u u :r r :b b :to-px T) (cell-margins layout)
      (let* ((w (pxw size))
             (h (pxh size))
             (th 0.0)
             (min-h (pxh (min-size layout)))
             (min-w (if (stretch layout) (- w l r) (pxw (min-size layout)))))
        (do-elements (element layout :result (px-size w th))
          (let* ((size (suggest-size (px-size min-w min-h) element))
                 (ew (pxw size))
                 (eh (if (= 0 (pxh size)) 0 (+ (max min-h (pxh size)) u b))))
            (setf w (max w (+ ew l r)))
            (incf th eh)))))))

(defmethod refit ((layout vertical-linear-layout))
  (when (layout-tree layout)
    (with-unit-parent layout
      (destructure-margins (:l l :u u :r r :b b :to-px T) (cell-margins layout)
        (destructure-extent (:w w :h h :to-px T) (bounds layout)
          (let ((mh (pxh (min-size layout)))
                (mw (if (stretch layout) (- w l r) (pxw (min-size layout))))
                (y (ecase (align layout)
                     ((:start :bottom) b)
                     ((:end :top) (- h u))
                     (:middle (- (* 0.5 (+ h (pxh (suggest-size (bounds layout) layout)))) u))))
                (x l))
            (do-elements (element layout :result (px-extent x 0 w y))
              (let* ((size (suggest-size (px-size mw mh) element))
                     ;; Stretch if necessary.
                     (ew (max mw (pxw size)))
                     (eh (pxh size)))
                (cond ((< 0 eh)
                       (setf eh (max mh eh))
                       (setf (bounds element)
                             (px-extent x
                                        (ecase (align layout)
                                          ((:start :bottom) y)
                                          ((:end :top :middle) (- y eh)))
                                        (min (- w l r) ew)
                                        eh))
                       (setf eh (+ u b (pxh (bounds element)))))
                      (T
                       (setf (bounds element) (px-extent 0 0 0 0))))
                (ecase (align layout)
                  ((:start :bottom) (incf y eh))
                  ((:end :top :middle) (decf y eh)))))))))))

(defclass horizontal-linear-layout (linear-layout)
  ())

(defmethod suggest-size (size (layout horizontal-linear-layout))
  (with-unit-parent layout
    (destructure-margins (:l l :u u :r r :b b :to-px T) (cell-margins layout)
      (let* ((w (pxw size))
             (h (pxh size))
             (tw 0.0)
             (mw (pxw (min-size layout)))
             (mh (if (stretch layout) (- h u b) (pxh (min-size layout)))))
        (do-elements (element layout :start 0 :result (px-size tw h))
          (let* ((size (suggest-size (px-size mw mh) element))
                 (ew (if (= 0 (pxw size)) 0 (+ (max mw (pxw size)) l r)))
                 (eh (pxh size)))
            (setf h (max h (+ eh u b)))
            (incf tw ew)))))))

(defmethod refit ((layout horizontal-linear-layout))
  (with-unit-parent layout
    (destructure-margins (:l l :u u :r r :b b :to-px T) (cell-margins layout)
      (destructure-extent (:w w :h h :to-px T) (bounds layout)
        (let ((mw (pxw (min-size layout)))
              (mh (if (stretch layout) (- h u b) (pxh (min-size layout))))
              (x (ecase (align layout)
                   ((:start :left) l)
                   ((:end :right) (- w r))
                   (:middle (* 0.5 (+ w (pxw (suggest-size (bounds layout) layout)))))))
              (y b))
          (do-elements (element layout :result (px-extent 0 y x h))
            (let* ((size (suggest-size (px-size mw mh) element))
                   (eh (pxh size))
                   (ew (pxw size)))
              (cond ((< 0 ew)
                     (setf ew (max ew mw))
                     (setf (bounds element)
                           (px-extent (ecase (align layout)
                                        ((:start :left :middle) x)
                                        ((:end :right) (- x ew)))
                                      y
                                      ew
                                      (min (- h u b) eh)))
                     (setf ew (+ l r (pxw (bounds element)))))
                    (T
                     (setf (bounds element) (px-extent 0 0 0 0))))
              (ecase (align layout)
                ((:start :left :middle) (incf x ew))
                ((:end :right) (decf x ew))))))))))
