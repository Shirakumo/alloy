(in-package #:org.shirakumo.alloy.renderers.simple.presentations)

;; TODO: Pallettes

(defclass default-look-and-feel (renderer)
  ())

;;; Sizing strategies
;;;
;;; We define specialized sizing strategies here that work in terms of shapes
;;; (The point of having "sizing strategies" is to let renderers control the
;;; size computations without the layout module having to know about the details
;;; of the specific renderer).

;;; Compute the component size based on some or all shapes that represent the
;;; component graphically.
(defclass fit-to-shapes (alloy:fit-to-content)
  (;; A list of shape names that should be considered when computing the size of
   ;; the renderable layout element. The value T (instead of a list) means that
   ;; all shapes should be considered. Any other non-NIL symbol (instead of a
   ;; list) specifies the name of a single shape that should be considered for
   ;; the computation. Specified shape names that do not correspond to shapes
   ;; present in the renderable are ignored.
   (shapes :initarg :shapes
           :reader shapes
           :initform T)))

(defmethod print-object ((object fit-to-shapes) stream)
  (print-unreadable-object (object stream :type T :identity T)
    (let ((shapes (shapes object)))
      (typecase shapes
        ((eql T) (write-string "all" stream))
        (symbol (prin1 shapes stream))
        (list (format stream "~{~S~^ ~}" shapes))))))

(defmethod alloy:compute-ideal-size :around ((layout-element alloy:renderable) (sizing-strategy fit-to-shapes) (size alloy:size))
  (alloy:with-unit-parent layout-element
    (call-next-method)))

(defmethod alloy:compute-ideal-size ((layout-element alloy:renderable) (sizing-strategy fit-to-shapes) (size alloy:size))
  (let ((shapes (shapes sizing-strategy))
        (min-x 0)
        (min-y 0)
        (max-x NIL)
        (max-y NIL))
    (flet ((process-shape (shape)
             (when shape
               (let* ((shape-min-x 0)
                      (shape-min-y 0)
                      (shape-size (or (alloy:suggest-size (alloy:ensure-extent (simple:bounds shape) size) shape)
                                      (load-time-value (alloy:px-size) t)))
                      (shape-width (alloy:pxw shape-size))
                      (shape-height (alloy:pxh shape-size))
                      (shape-max-x (+ shape-min-x shape-width))
                      (shape-max-y (+ shape-min-y shape-height)))
                 (when (typep shape-size 'alloy:extent)
                   (let ((x (alloy:pxx shape-size))
                         (y (alloy:pxy shape-size)))
                     (incf shape-min-x (max 0 x))
                     (incf shape-min-y (max 0 y))
                     (incf shape-max-x x)
                     (incf shape-max-y y)))
                 (setf min-x (min min-x shape-min-x))
                 (setf min-y (min min-y shape-min-y))
                 (setf max-x (if (null max-x) shape-max-x (max max-x shape-max-x)))
                 (setf max-y (if (null max-y) shape-max-y (max max-y shape-max-y)))))))
      (typecase shapes
        ((eql T) (loop for (nil . shape) across (shapes layout-element)
                       do (process-shape shape)))
        (symbol (process-shape (find-shape shapes layout-element)))
        (otherwise (loop for name in shapes
                         for shape = (find-shape name layout-element)
                         do (process-shape shape)))))
    (if max-x
        (alloy:px-size (- max-x min-x) (- max-y min-y))
        size)))

;;; The sizing strategy is stateless so for each value of the :SHAPES initarg, a
;;; single instance can be shared by all users.
(defvar *fit-to-all-shapes* (make-instance 'fit-to-shapes))

(defvar *fit-to-label* (make-instance 'alloy:at-least
                                      :minimum-size (make-instance 'fit-to-shapes
                                                                   :shapes :label)))

;;; Defaults

(define-update (default-look-and-feel alloy:renderable)
  (:background
   :pattern (colored:color 0.15 0.15 0.15))
  (:border
   :pattern (colored:color 0.9 0.9 0.9)
   :hidden-p (null alloy:focus)
   :z-index 1)
  (:label
   :pattern (case alloy:focus
              ((:weak :strong) colors:black)
              (T colors:white))))

(defmethod compute-sizing-strategy ((renderer default-look-and-feel) (renderable alloy:renderable))
  *fit-to-all-shapes*)

;;;

(define-realization (default-look-and-feel alloy:tooltip)
  ((:background simple:rectangle)
   (alloy:margins)
   :pattern colors:black
   :z-index 1000)
  ((:label simple:text)
   (alloy:margins 2)
   alloy:text
   :pattern colors:white
   :wrap NIL
   :halign :start :valign :middle
   :z-index 1000))

(define-update (default-look-and-feel alloy:tooltip)
  (:background
   :pattern colors:black)
  (:label
   :text alloy:text))

(defmethod compute-sizing-strategy ((renderer default-look-and-feel) (renderable alloy:tooltip))
  *fit-to-label*)

(define-realization (default-look-and-feel alloy:label)
  ((:background simple:rectangle)
   (alloy:margins)
   :pattern colors:transparent)
  ((:label simple:text)
   (alloy:margins)
   alloy:text
   :pattern colors:white
   :wrap (alloy:wrap alloy:renderable)
   :halign :start :valign :middle
   :z-index 1))

(define-update (default-look-and-feel alloy:label)
  (:background
   :pattern colors:transparent) ; overwrite inherited behavior
  (:label
   :text alloy:text
   :pattern colors:white)) ; overwrite inherited behavior

(defmethod compute-sizing-strategy ((renderer default-look-and-feel) (renderable alloy:label))
  *fit-to-label*)

(define-realization (default-look-and-feel alloy::list-label)
  ((:background simple:rectangle)
   (alloy:margins))
  ((:label simple:text)
   (alloy:margins)
   alloy:text
   :pattern colors:white
   :halign :start :valign :middle))

(define-update (default-look-and-feel alloy::list-label)
  (:label
   :pattern colors:white)
  (:background
   :pattern (case alloy:focus
              (:strong (colored:color 0.25 0.2 0.8))
              (:weak (colored:color 0.25 0.25 0.25))
              (T colors:transparent))))

(define-realization (default-look-and-feel alloy:icon)
  ((:icon simple:icon)
   (alloy:margins)
   alloy:value))

(define-realization (default-look-and-feel alloy:button)
  ((:background simple:rectangle)
   (alloy:margins))
  ((:border simple:rectangle)
   (alloy:margins -3)
   :line-width (alloy:un 1))
  ((:label simple:text)
   (alloy:margins 1)
   alloy:text
   :halign :middle
   :valign :middle))

(define-update (default-look-and-feel alloy:button)
  (:background
   :pattern (case alloy:focus
              (:strong (colored:color 0.9 0.9 0.9))
              (:weak (colored:color 0.7 0.7 0.7))
              (T (colored:color 0.25 0.2 0.8))))
  (:label
   :text alloy:text))

(defmethod compute-sizing-strategy ((renderer default-look-and-feel) (renderable alloy:button))
  *fit-to-label*)

(define-realization (default-look-and-feel alloy:switch)
  ((:background simple:rectangle)
   (alloy:margins))
  ((:border simple:rectangle)
   (alloy:margins -3)
   :line-width (alloy:un 1))
  ((:switch simple:rectangle)
   (alloy:extent 0 0 (alloy:pw 0.3) (alloy:ph))))

(define-update (default-look-and-feel alloy:switch)
  (:switch
   :offset (alloy:point (if (alloy:active-p alloy:renderable)
                            (alloy:pw 0.7)
                            0))
   :pattern (case alloy:focus
              (:strong (colored:color 0.9 0.9 0.9))
              (:weak (colored:color 0.7 0.7 0.7))
              (T (colored:color 0.25 0.2 0.8)))))

(defvar *at-least-32x32*
  (make-instance 'alloy:at-least :minimum-size (alloy:size 32 32)))

(defmethod compute-sizing-strategy ((renderer default-look-and-feel) (renderable alloy:switch))
  *at-least-32x32*)

(define-realization (default-look-and-feel alloy:labelled-switch T)
  ((:label simple:text)
   (alloy:margins 1)
   alloy:text
   :halign :middle
   :valign :middle))

(define-update (default-look-and-feel alloy:labelled-switch)
  (:label
   :text alloy:text
   :pattern colors:white)) ; override inherited behavior

(defmethod compute-sizing-strategy ((renderer default-look-and-feel) (renderable alloy:labelled-switch))
  (make-instance 'fit-to-shapes :shapes :label))

(define-realization (default-look-and-feel alloy:checkbox)
  ((:background simple:rectangle)
   (alloy:margins))
  ((:border simple:rectangle)
   (alloy:margins -3)
   :line-width (alloy:un 1))
  ((:button simple:rectangle)
   (alloy:margins 3)))

(define-update (default-look-and-feel alloy:checkbox)
  (:button
   :hidden-p (not (alloy:active-p alloy:renderable))
   :pattern (case alloy:focus
              ((NIL) (colored:color 0.25 0.25 0.25))
              (T (colored:color 0.25 0.2 0.8)))))

(define-realization (default-look-and-feel alloy:text-input-component)
  ((:background simple:rectangle)
   (alloy:margins))
  ((:border simple:rectangle)
   (alloy:margins -3)
   :line-width (alloy:un 1))
  ((:label simple:text)
   (alloy:margins 1)
   alloy:text)
  ((:cursor simple:cursor)
   (find-shape :label alloy:renderable)
   0
   :composite-mode :source-over
   :pattern colors:black)
  ((:selection simple:selection)
   (find-shape :label alloy:renderable)
   0 0)
  ((:placeholder simple:text)
   (alloy:margins 1)
   (alloy:placeholder alloy:renderable)))

(define-update (default-look-and-feel alloy:text-input-component)
  (:background
   :pattern (case alloy:focus
              (:strong (colored:color 0.9 0.9 0.9))
              (:weak (colored:color 0.7 0.7 0.7))
              (T (colored:color 0.2 0.2 0.2))))
  (:label
   :text alloy:text)
  (:cursor
   :hidden-p (null alloy:focus)
   :start (alloy:pos (alloy:cursor alloy:renderable)))
  (:selection
   :hidden-p (null (alloy:anchor (alloy:cursor alloy:renderable)))
   :start (min (or (alloy:anchor (alloy:cursor alloy:renderable)) 0)
               (alloy:pos (alloy:cursor alloy:renderable)))
   :end (max (or (alloy:anchor (alloy:cursor alloy:renderable)) 0)
             (alloy:pos (alloy:cursor alloy:renderable))))
  (:placeholder
   :text (alloy:placeholder alloy:renderable)
   :hidden-p (/= 0 (length alloy:text))
   :pattern (colored:color 1 1 1 0.5)))

(defmethod compute-sizing-strategy ((renderer default-look-and-feel) (renderable alloy:text-input-component))
  *fit-to-label*)

(define-update (default-look-and-feel alloy:input-box)
  (:label
   :valign :top
   :wrap T)
  (:placeholder
   :valign :top
   :wrap T))

(define-realization (default-look-and-feel alloy:validated-text-input T)
  ((:invalid-marker simple:rectangle)
   (alloy:extent 0 -2 (alloy:pw 1) 2)
   :pattern colors:red))

(define-update (default-look-and-feel alloy:validated-text-input)
  (:invalid-marker
   :hidden-p (alloy:valid-p alloy:renderable alloy:text)))

(define-realization (default-look-and-feel alloy:slider)
  ((:background simple:rectangle)
   (ecase (alloy:orientation alloy:renderable)
     (:horizontal (alloy:extent 0 (alloy:ph 0.4) (alloy:pw) (alloy:ph 0.2)))
     (:vertical (alloy:extent (alloy:pw 0.4) 0 (alloy:pw 0.2) (alloy:ph)))))
  ((:border simple:rectangle)
   (alloy:margins -3)
   :line-width (alloy:un 1))
  ((:handle simple:rectangle)
   (ecase (alloy:orientation alloy:renderable)
     (:horizontal (alloy:extent -5 0 10 (alloy:ph)))
     (:vertical (alloy:extent 0 -5 (alloy:pw) 10)))))

(define-update (default-look-and-feel alloy:slider)
  (:handle
   :offset (ecase (alloy:orientation alloy:renderable)
             (:horizontal (alloy:point (alloy:pw (alloy:slider-unit alloy:renderable)) 0))
             (:vertical (alloy:point 0 (alloy:ph (alloy:slider-unit alloy:renderable)))))
   :pattern (case alloy:focus
                 (:strong colors:white)
                 (T (colored:color 0.25 0.2 0.8)))))

(define-realization (default-look-and-feel alloy:progress)
  ((:background simple:rectangle)
   (alloy:margins))
  ((:bar simple:rectangle)
   (alloy:margins 3))
  ((:label simple:text)
   (alloy:margins 1)
   ""
   :halign :middle
   :valign :middle))

(define-update (default-look-and-feel alloy:progress)
  (:bar
   :pattern (colored:color 0.25 0.2 0.8)
   :scale (let ((p (/ alloy:value (alloy:maximum alloy:renderable))))
            (alloy:px-size p 1)))
  (:label
   :text (format NIL "~,1f%" (/ alloy:value (alloy:maximum alloy:renderable) 1/100))
   :pattern colors:white))

(define-realization (default-look-and-feel alloy:radio)
  ((:background simple:ellipse)
   (alloy:extent 0 0 (alloy:ph 1) (alloy:ph 1)))
  ((:inner simple:ellipse)
   (alloy:extent (alloy:ph 0.1) (alloy:ph 0.1) (alloy:ph 0.8) (alloy:ph 0.8)))
  ((:border simple:ellipse)
   (alloy:extent (alloy:ph -0.1) (alloy:ph -0.1) (alloy:ph 1.2) (alloy:ph 1.2))
   :line-width (alloy:un 1)))

(define-update (default-look-and-feel alloy:radio)
  (:inner
   :hidden-p (not (alloy:active-p alloy:renderable))
   :pattern (colored:color 0.25 0.2 0.8)))

(define-realization (default-look-and-feel alloy:combo)
  ((:background simple:rectangle)
   (alloy:margins))
  ((:border simple:rectangle)
   (alloy:margins -3)
   :line-width (alloy:un 1))
  ((:label simple:text)
   (alloy:margins 1)
   alloy:text
   :valign :middle))

(define-update (default-look-and-feel alloy:combo)
  (:label
   :pattern colors:white
   :text alloy:text))

(defmethod compute-sizing-strategy ((renderer default-look-and-feel) (renderable alloy:combo))
  *fit-to-label*)

(define-realization (default-look-and-feel alloy:combo-item)
  ((:background simple:rectangle)
   (alloy:margins)
   :z-index 100)
  ((:label simple:text)
   (alloy:margins 1)
   alloy:text
   :z-index 100))

(define-update (default-look-and-feel alloy:combo-item)
  (:background
   :pattern (case (alloy:focus alloy:renderable)
                 ((:weak :strong) (colored:color 0.25 0.2 0.8))
                 ((NIL) (colored:color 0.15 0.15 0.15))))
  (:label
   :pattern colors:white))

(define-realization (default-look-and-feel alloy:scrollbar)
  ((:background simple:rectangle)
   (alloy:margins))
  ((:handle simple:rectangle)
   (ecase (alloy:orientation alloy:renderable)
     (:horizontal (alloy:extent -10 0 20 (alloy:ph)))
     (:vertical (alloy:extent 0 -10 (alloy:pw) 20)))))

(define-update (default-look-and-feel alloy:scrollbar)
  (:handle
   :pattern (case alloy:focus
              (:strong (colored:color 0.5 0.5 0.5))
              (T (colored:color 0.2 0.2 0.2)))))

(defmethod alloy:render :before ((renderer renderer) (component alloy:scrollbar))
  (alloy:constrain-visibility component renderer))

(define-realization (default-look-and-feel alloy:plot)
  ((:background simple:rectangle)
   (alloy:margins))
  ((:border simple:rectangle)
   (alloy:margins -3)
   :line-width (alloy:un 1))
  ((:curve simple:line-strip)
   (alloy:plot-points alloy:renderable)
   :pattern (colored:color 0.25 0.2 0.8)
   :line-width (alloy:un 2)))

(define-update (default-look-and-feel alloy:plot)
  (:curve
   :points (alloy:plot-points alloy:renderable)))

(define-realization (default-look-and-feel alloy:tab-button)
  ((:background simple:polygon)
   (list (alloy:point 0 0) (alloy:point 1 0)
         (alloy:point 0.9 1) (alloy:point 0.1 1))
   :scale (alloy:size (alloy:pw 1) (alloy:ph 1)))
  ((:label simple:text)
   (alloy:margins 1)
   (alloy:name (alloy:active-value alloy:renderable))
   :halign :middle
   :valign :middle))

(define-update (default-look-and-feel alloy:tab-button)
  (:background
   :pattern (case alloy:focus
              (:strong (colored:color 0.9 0.9 0.9))
              (:weak (colored:color 0.7 0.7 0.7))
              (T (if (alloy:active-p alloy:renderable)
                     (colored:color 0.25 0.2 0.8)
                     (colored:color 0.2 0.2 0.2))))))

(defmethod compute-sizing-strategy ((renderer default-look-and-feel) (renderable alloy:tab-button))
  *fit-to-label*)

(define-realization (default-look-and-feel alloy::window-title)
  ((:label simple:text)
   (alloy:margins)
   alloy:text
   :pattern colors:white
   :halign :middle :valign :middle))

(define-update (default-look-and-feel alloy::window-title)
  (:label
   :pattern colors:white))

(define-realization (default-look-and-feel alloy::frame)
  ((:background simple:rectangle)
   (alloy:margins)
   :pattern (colored:color 0.1 0.1 0.1))
  ((:frame-border simple:rectangle)
   (alloy:margins)
   :line-width (alloy:un 1)
   :pattern colors:black))

(define-update (default-look-and-feel alloy::frame)
  (:background
   :pattern (colored:color 0.1 0.1 0.1)))

(define-realization (default-look-and-feel alloy:menubar)
  ((:background simple:rectangle)
   (alloy:margins)
   :pattern (colored:color 0.05 0.05 0.05)))

(define-update (default-look-and-feel alloy:menubar)
  ((:background simple:rectangle)
   :pattern (colored:color 0.1 0.1 0.1)))

(define-realization (default-look-and-feel alloy:submenu)
  ((:background simple:rectangle)
   (alloy:margins)
   :pattern (colored:color 0.1 0.1 0.1)
   :z-index 1000))

(define-realization (default-look-and-feel alloy:menu-item)
  ((:background simple:rectangle)
   (alloy:margins)
   :z-index 1000)
  ((:label simple:text)
   (alloy:margins 2 1) alloy:text
   :halign :start :valign :middle
   :z-index 1000))

(define-update (default-look-and-feel alloy:menu-item)
  (:background
   :pattern (case alloy:focus
              (:strong (colored:color 0.5 0.5 0.5))
              (:weak (colored:color 0.3 0.3 0.3))
              (T colors:transparent)))
  (:label
   :text alloy:text
   :pattern colors:white))

(define-realization (default-look-and-feel alloy:separator)
  ((:line simple:rectangle)
   (alloy:extent 0 (alloy:ph 0.5) (alloy:pw) 1)
   :pattern (colored:color 0.2 0.2 0.2)
   :z-index 1000))

(define-realization (default-look-and-feel alloy:resizer)
  ((:line simple:rectangle)
   (ecase (alloy:side alloy:renderable)
     ((:north :south) (alloy:extent 0 (alloy:ph 0.5) (alloy:pw) 1))
     ((:east :west) (alloy:extent (alloy:pw 0.5) 0 1 (alloy:ph))))
   :pattern (colored:color 0.2 0.2 0.2)))
