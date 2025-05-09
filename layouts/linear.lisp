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
             (min-w (if (stretch layout) (- w l r) (pxw (min-size layout))))
             (y (ecase (align layout)
                  (:start b)
                  (:end (- h u)))))
        (do-elements (element layout :result (px-size w (max h th)))
          (let* ((size (suggest-size (px-size min-w min-h) element))
                 (ew (pxw size))
                 (eh (if (= 0 (pxh size)) 0 (+ (max min-h (pxh size)) u b))))
            (setf w (max w (+ ew l r)))
            (incf th eh)
            (ecase (align layout)
              (:start (incf y eh))
              (:end (decf y eh)))))))))

(defmethod refit ((layout vertical-linear-layout))
  (when (layout-tree layout)
    (with-unit-parent layout
      (destructure-margins (:l l :u u :r r :b b :to-px T) (cell-margins layout)
        (destructure-extent (:w w :h h :to-px T) (bounds layout)
          (let ((mh (pxh (min-size layout)))
                (mw (if (stretch layout) (- w l r) (pxw (min-size layout))))
                (y (ecase (align layout)
                     (:start b)
                     (:end (- h u))))
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
                                          (:start y)
                                          (:end (- y eh)))
                                        (min (- w l r) ew)
                                        eh))
                       (setf eh (+ u b (pxh (bounds element)))))
                      (T
                       (setf (bounds element) (px-extent 0 0 0 0))))
                (ecase (align layout)
                  (:start (incf y eh))
                  (:end (decf y eh)))))))))))

(defclass horizontal-linear-layout (linear-layout)
  ())

(defmethod suggest-size (size (layout horizontal-linear-layout))
  (with-unit-parent layout
    (destructure-margins (:l l :u u :r r :b b :to-px T) (cell-margins layout)
      (let* ((w (pxw size))
             (h (pxh size))
             (tw 0.0)
             (mw (pxw (min-size layout)))
             (mh (if (stretch layout) (- h u b) (pxh (min-size layout))))
             (x (ecase (align layout)
                  (:start l)
                  (:end (- w r)))))
        (do-elements (element layout :start 0 :result (px-size (max w tw) h))
          (let* ((size (suggest-size (px-size mw mh) element))
                 (ew (if (= 0 (pxw size)) 0 (+ (max mw (pxw size)) l r)))
                 (eh (pxh size)))
            (setf h (max h (+ eh u b)))
            (incf tw ew)
            (ecase (align layout)
              (:start (incf x ew))
              (:end (decf x ew)))))))))

(defmethod refit ((layout horizontal-linear-layout))
  (with-unit-parent layout
    (destructure-margins (:l l :u u :r r :b b :to-px T) (cell-margins layout)
      (destructure-extent (:w w :h h :to-px T) (bounds layout)
        (let ((mw (pxw (min-size layout)))
              (mh (if (stretch layout) (- h u b) (pxh (min-size layout))))
              (x (ecase (align layout)
                   (:start l)
                   (:end (- w r))))
              (y b))
          (do-elements (element layout :result (px-extent 0 y x h))
            (let* ((size (suggest-size (px-size mw mh) element))
                   (eh (pxh size))
                   (ew (pxw size)))
              (cond ((< 0 ew)
                     (setf ew (max ew mw))
                     (setf (bounds element)
                           (px-extent (ecase (align layout)
                                        (:start x)
                                        (:end (- x ew)))
                                      y
                                      ew
                                      (min (- h u b) eh)))
                     (setf ew (+ l r (pxw (bounds element)))))
                    (T
                     (setf (bounds element) (px-extent 0 0 0 0))))
              (ecase (align layout)
                (:start (incf x ew))
                (:end (decf x ew))))))))))
