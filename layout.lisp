(in-package #:org.shirakumo.alloy)

(defgeneric layout-tree (layout-element))
(defgeneric layout-parent (layout-element))
(defgeneric notice-size (changed parent))
(defgeneric suggest-size (size layout-element))
(defgeneric sizing-strategy (layout-element))
(defgeneric (setf sizing-strategy) (new-strategy layout-element))
(defgeneric compute-ideal-size (layout-element sizing-strategy size))
(defgeneric ensure-visible (element parent))
(defgeneric bounds (layout-element))
(defgeneric (setf bounds) (extent layout-element))
(defgeneric resize (element w h))
(defgeneric refit (layout))
(defgeneric location (element))
(defgeneric (setf location) (location element))
(defgeneric global-location (layout-element))
(defgeneric (setf global-location) (extent layout-element))

;;; Sizing strategies

(defclass sizing-strategy () ())

(defclass fixed-size (sizing-strategy)
  ((fixed-size :initform (arg! :fixed-size) :initarg :fixed-size :reader fixed-size)))

(defmethod compute-ideal-size ((layout-element T) (sizing-strategy fixed-size) (size size))
  (fixed-size sizing-strategy))

(defclass dynamic-size (sizing-strategy)
  ((size-function :initform (arg! :size-function) :initarg :size-function :reader size-function)))

(defmethod compute-ideal-size ((layout-element T) (sizing-strategy dynamic-size) (size size))
  (funcall (size-function sizing-strategy) layout-element size))

(defclass proportional (sizing-strategy)
  ((aspect-ratio :initform 1.0 :initarg :aspect-ratio :accessor aspect-ratio)))

(defmethod compute-ideal-size ((layout-element T) (sizing-strategy proportional) (size size))
  (let ((ratio (aspect-ratio sizing-strategy)))
    (if (< 1.0 ratio)
        (px-size (pxw size) (/ (pxw size) ratio))
        (px-size (* (pxh size) ratio) (pxh size)))))

(defclass dont-care (sizing-strategy) ())

(defmethod compute-ideal-size ((layout-element T) (sizing-strategy dont-care) (size size))
  size)

;;; Since the strategy has no state, there is no need for more than instance.
(defvar *dont-care* (make-instance 'dont-care))

(defclass at-least (sizing-strategy)
  ((minimum-size :initform (arg! :minimum-size) :initarg :minimum-size :reader minimum-size)))

(defmethod compute-ideal-size ((layout-element T) (sizing-strategy at-least) (size size))
  (let* ((minimum-size (minimum-size sizing-strategy))
         (minimum (etypecase minimum-size
                    (size minimum-size)
                    (sizing-strategy (compute-ideal-size layout-element minimum-size size)))))
    (px-size (max (pxw minimum) (pxw size)) (max (pxh minimum) (pxh size)))))

(defclass fit-to-content (sizing-strategy) ())

(defmethod compute-ideal-size ((layout-element T) (sizing-strategy fit-to-content) (size size))
  (error "Renderer must implement this"))

;;; Layout element

;;; Renderers can recognize this specific object as indicating the absence of a
;;; user-supplied sizing strategy.
(defvar *fallback-sizing-strategy* (make-instance 'dont-care))

(defclass layout-element (element)
  ((layout-tree :initform NIL :reader layout-tree :writer set-layout-tree)
   (layout-parent :reader layout-parent)
   (bounds :initform (%extent (px 0) (px 0) (px 0) (px 0)) :reader bounds)
   (sizing-strategy :initform *fallback-sizing-strategy*
                    :accessor sizing-strategy)))

(defmethod initialize-instance :after ((element layout-element) &key layout-parent)
  (when layout-parent
    (enter element layout-parent)))

(defmethod shared-initialize :after ((element layout-element) (slot-names T)
                                     &key sizing-strategy ideal-size)
  (cond (sizing-strategy
         (setf (sizing-strategy element)
               (typecase sizing-strategy
                 (function (make-instance 'dynamic-size :size-function sizing-strategy))
                 (T sizing-strategy))))
        (ideal-size
         (setf (sizing-strategy element)
               (make-instance 'fixed-size :fixed-size ideal-size)))))

(defmethod print-object ((element layout-element) stream)
  (print-unreadable-object (element stream :type T :identity T)
    (format stream "~a" (bounds element))))

(defmethod refresh ((element layout-element)))

(defmethod ui ((element layout-element))
  (ui (layout-tree element)))

(defmethod x ((element layout-element)) (extent-x (bounds element)))
(defmethod y ((element layout-element)) (extent-y (bounds element)))
(defmethod w ((element layout-element)) (extent-w (bounds element)))
(defmethod h ((element layout-element)) (extent-h (bounds element)))

(defmethod (setf x) ((unit unit) (element layout-element))
  (setf (extent-x (bounds element)) unit))
(defmethod (setf y) ((unit unit) (element layout-element))
  (setf (extent-y (bounds element)) unit))
(defmethod (setf w) ((unit unit) (element layout-element))
  (setf (extent-w (bounds element)) unit))
(defmethod (setf h) ((unit unit) (element layout-element))
  (setf (extent-h (bounds element)) unit))

(defmethod (setf x) ((real real) (element layout-element))
  (setf (extent-x (bounds element)) (px real)))
(defmethod (setf y) ((real real) (element layout-element))
  (setf (extent-y (bounds element)) (px real)))
(defmethod (setf w) ((real real) (element layout-element))
  (setf (extent-w (bounds element)) (px real)))
(defmethod (setf h) ((real real) (element layout-element))
  (setf (extent-h (bounds element)) (px real)))

(defmethod (setf bounds) ((extent extent) (element layout-element))
  (let ((bounds (bounds element)))
    (setf (extent-x bounds) (extent-x extent))
    (setf (extent-y bounds) (extent-y extent))
    (setf (extent-w bounds) (extent-w extent))
    (setf (extent-h bounds) (extent-h extent))
    extent))

(defmethod (setf bounds) ((size size) (element layout-element))
  (let ((bounds (bounds element)))
    (setf (extent-w bounds) (size-w size))
    (setf (extent-h bounds) (size-h size))
    size))

(defmethod location ((element layout-element))
  (point (extent-x (bounds element)) (extent-y (bounds element))))

(defmethod (setf location) ((point point) (element layout-element))
  (setf (extent-x (bounds element)) (point-x point))
  (setf (extent-y (bounds element)) (point-y point))
  point)

(defmethod resize ((element layout-element) (w unit) (h unit))
  (setf (extent-w (bounds element)) w)
  (setf (extent-h (bounds element)) h)
  (when (layout-tree element)
    (notice-size element (layout-parent element)))
  element)

(defmethod notice-size ((element layout-element) (parent (eql T)))
  (when (and (slot-boundp element 'layout-parent)
             (not (eq element (layout-parent element)))
             (layout-tree element))
    (notice-size element (layout-parent element))))

;; Default to minimal size
(defmethod preferred-size ((element layout-element))
  (suggest-size (size) element))

;; KLUDGE: We can't actually elide this as the layout might prefer a *different*
;;         size to what it already has right now, and we need to reconsider it.
#++
(defmethod suggest-size :around ((size size) (element layout-element))
  (if (and (u= (size-w size) (extent-w (bounds element)))
           (u= (size-h size) (extent-h (bounds element))))
      size
      (call-next-method)))

;; KLUDGE: The specializer for the SIZE parameter used to be SIZE. We changed it
;;         to T so that this method does not take precedence over
;;         element-specific methods (that is more specialized in the ELEMENT
;;         parameter) which use (size T) for the first parameter.
(defmethod suggest-size ((size T) (element layout-element))
  (compute-ideal-size element (sizing-strategy element) size))

(defmethod (setf sizing-strategy) :after ((new-strategy T) (element layout-element))
  (notice-size element T))

(defmethod (setf bounds) :around ((extent extent) (element layout-element))
  ;; No need to actually pass through if we're re-using the current size,
  ;; as it's already been stabilised before.
  (let ((bounds (bounds element)))
    (if (and (u= (extent-w extent) (extent-w bounds))
             (u= (extent-h extent) (extent-h bounds)))
        (setf (extent-x bounds) (extent-x extent)
              (extent-y bounds) (extent-y extent))
        (call-next-method))))

(defmethod (setf bounds) :around ((size size) (element layout-element))
  ;; No need to actually pass through if we're re-using the current size,
  ;; as it's already been stabilised before.
  (let ((bounds (bounds element)))
    (with-unit-parent element
      (when (or (u/= (size-w size) (extent-w bounds))
                (u/= (size-h size) (extent-h bounds)))
        (call-next-method)))))

(defmethod compute-global-position ((element layout-element))
  (let ((x 0f0) (y 0f0))
    (when (layout-tree element)
      (with-unit-parent element
        (loop for current = element then parent
              for parent = (layout-parent current)
              do (incf x (pxx (bounds current)))
                 (incf y (pxy (bounds current)))
                 ;; KLUDGE: wish this didn't have to be like this.
                 (when (typep current 'clip-view)
                   (incf x (pxx (offset current)))
                   (incf y (pxy (offset current))))
              until (eql current parent))))
    (values x y)))

(defmacro with-global-bounds ((bounds element) &body body)
  (let ((x (gensym "X"))
        (y (gensym "Y")))
    `(multiple-value-bind (,x ,y) (compute-global-position ,element)
       (let* ((,x (%px ,x))
              (,y (%px ,y))
              (,bounds (bounds ,element))
              (,bounds (%extent ,x ,y (extent-w ,bounds) (extent-h ,bounds))))
         (declare (dynamic-extent ,x ,y ,bounds))
         ,@body))))

(defmethod global-location ((element layout-element))
  (multiple-value-bind (x y) (compute-global-position element)
    (px-point x y)))

(defmethod (setf global-location) ((location point) (element layout-element))
  (with-unit-parent element
    (multiple-value-bind (x y) (compute-global-position element)
      (let* ((parent-x (- x (pxx element)))
             (parent-y (- y (pxy element)))
             (new-parent-x (- (pxx location) parent-x))
             (new-parent-y (- (pxy location) parent-y)))
        (setf (extent-x (bounds element)) (px new-parent-x))
        (setf (extent-y (bounds element)) (px new-parent-y))
        location))))

(defmethod contained-p (thing (element layout-element))
  (with-global-bounds (bounds element)
    (contained-p thing bounds)))

(defmethod constrain-visibility ((element layout-element) (renderer renderer))
  (with-global-bounds (bounds element)
    (constrain-visibility bounds renderer)))

(defmethod set-layout-tree :before (tree (element layout-element))
  (when (and (layout-tree element) tree (not (eq tree (layout-tree element))))
    (error 'element-has-different-root
           :bad-element element :container tree)))

(defmethod extent-visible-p ((element layout-element) (renderer renderer))
  (with-global-bounds (bounds element)
    (extent-visible-p bounds renderer)))

(defmethod handle ((event event) (element layout-element))
  (decline))

(defmethod handle :around ((event event) (element layout-element))
  (with-unit-parent element
    (call-next-method)))

(defmethod render :around ((renderer renderer) (element layout-element))
  (with-unit-parent element
    (when (and (layout-tree element)
               (extent-visible-p element renderer))
      (call-next-method))))

(defmethod register :around ((element layout-element) (renderer renderer))
  (with-unit-parent element
    (call-next-method)))

(defmethod ensure-visible (element (parent layout-element))
  (when (and (slot-boundp parent 'layout-parent)
             (not (eq parent (layout-parent parent))))
    (ensure-visible element (layout-parent parent))))

(defmethod ensure-visible ((element layout-element) (parent (eql T)))
  (when (slot-boundp element 'layout-parent)
    (ensure-visible element (layout-parent element))))

(defmethod leave ((element layout-element) (parent (eql T)))
  (when (slot-boundp element 'layout-parent)
    (leave element (layout-parent element))))

(defclass layout (layout-element container renderable)
  ((layout-needed-p :initform T :accessor layout-needed-p)))

(defmethod print-object ((layout layout) stream)
  (print-unreadable-object (layout stream :type T :identity T)
    (format stream "~a~@[ DIRTY~]" (bounds layout) (layout-needed-p layout))))

(defmethod notice-size ((child layout-element) (layout layout))
  (setf (layout-needed-p layout) T))

(defmethod (setf bounds) :after ((extent extent) (layout layout))
  (setf (layout-needed-p layout) T))

(defmethod refit :around ((layout layout))
  (call-next-method)
  layout)

(defmethod refit :after ((layout layout))
  (setf (layout-needed-p layout) NIL)
  (do-elements (element layout)
    (when (and (typep element 'layout) (layout-needed-p element))
      (refit element))))

(defmethod refresh :after ((layout layout))
  (do-elements (element layout)
    (refresh element)))

(defmethod set-layout-tree :before (value (layout layout))
  (do-elements (element layout)
    (set-layout-tree value element)))

(defmethod set-layout-tree :after (value (layout layout))
  (unless value
    (setf (extent-w (bounds layout)) (px 0))
    (setf (extent-h (bounds layout)) (px 0))))

(defmethod enter :after ((element layout-element) (layout layout) &key)
  (when (layout-tree layout)
    (notice-size element layout)))

(defmethod enter :before ((element layout-element) (parent layout) &key)
  (cond ((not (slot-boundp element 'layout-parent))
         (set-layout-tree (layout-tree parent) element)
         (setf (slot-value element 'layout-parent) parent))
        ((not (eq parent (layout-parent element)))
         (restart-case
             (error 'element-has-different-parent
                    :bad-element element :container parent :parent (layout-parent element))
           (reparent ()
             :report "Leave the element from its current parent."
             (leave element T))))
        (T
         (error 'element-already-contained
                :bad-element element :container parent))))

(defmethod leave :before ((element layout-element) (parent layout))
  (unless (eq parent (layout-parent element))
    (error 'element-has-different-parent
           :bad-element element :container parent :parent (layout-parent element))))

(defmethod leave :after ((element layout-element) (parent layout))
  (set-layout-tree NIL element)
  (slot-makunbound element 'layout-parent))

(defmethod update :after ((element layout-element) (layout layout) &key)
  (when (layout-tree layout)
    (notice-size element layout)))

(defmethod element-index :before ((element layout-element) (layout layout))
  (unless (eq layout (layout-parent element))
    (error 'element-not-contained
           :bad-element element :container layout)))

(defmethod register :after ((layout layout) (renderer renderer))
  (do-elements (element layout)
    (register element renderer)))

(defmethod deregister :before ((layout layout) (renderer renderer))
  (do-elements (element layout)
    (deregister element renderer)))

(defmethod prepare-for-render :after ((layout layout) (renderer renderer))
  (do-elements (element layout)
    (prepare-for-render element renderer)))

(defmethod render :before ((renderer renderer) (layout layout))
  (when (layout-needed-p layout)
    (refit layout)))

(defmethod maybe-render :before ((renderer renderer) (layout layout))
  (when (layout-needed-p layout)
    (refit layout)))

(defmethod render ((renderer renderer) (layout layout))
  (do-elements (element layout)
    (render renderer element)))

(defmethod maybe-render ((renderer renderer) (layout layout))
  (do-elements (element layout)
    (maybe-render renderer element)))

(defmethod handle ((event layout-event) (layout layout))
  (do-elements (element layout :result (decline) :from-end T)
    (if (handle event element)
        (return)
        (decline))))

(defmethod handle ((event pointer-event) (layout layout))
  ;; Need to process in reverse order to ensure overlapping elements come first,
  ;; since elements drawn last overlap previous elements.
  (do-elements (element layout :result (decline) :from-end T)
    (when (contained-p (location event) element)
      (if (handle event element)
          (return)
          (decline)))))

(defmethod handle ((event pointer-move) (layout layout))
  ;; Need to process in reverse order to ensure overlapping elements come first,
  ;; since elements drawn last overlap previous elements.
  (do-elements (element layout :result (decline) :from-end T)
    (when (or (contained-p (location event) element)
              (contained-p (old-location event) element))
      (if (handle event element)
          (return)
          (decline)))))

(defclass layout-tree ()
  ((root :initform NIL :accessor root)
   (popups :initform (make-instance 'fixed-layout) :reader popups)
   (ui :initarg :ui :reader ui)))

(defmethod initialize-instance :after ((tree layout-tree) &key)
  (set-layout-tree tree (popups tree))
  (setf (slot-value (popups tree) 'layout-parent) (popups tree)))

(defmethod clear ((tree layout-tree))
  (setf (root tree) NIL)
  (clear (popups tree)))

(defmethod enter ((element layout-element) (tree layout-tree) &key force)
  (when (next-method-p) (call-next-method))
  (when force (setf (root tree) NIL))
  (setf (root tree) element))

(defmethod (setf root) :before ((none null) (tree layout-tree))
  (when (root tree)
    (set-layout-tree NIL (root tree))))

(defmethod (setf root) :before ((element layout-element) (tree layout-tree))
  (when (root tree)
    (error 'root-already-established
           :bad-element element :tree tree)))

(defmethod (setf root) :after ((element layout-element) (tree layout-tree))
  (set-layout-tree tree element)
  (setf (slot-value element 'layout-parent) element))

(defmethod register ((tree layout-tree) (renderer renderer))
  (register (root tree) renderer)
  (register (popups tree) renderer))

(defmethod deregister ((tree layout-tree) (renderer renderer))
  (deregister (root tree) renderer)
  (deregister (popups tree) renderer))

(defmethod render ((renderer renderer) (tree layout-tree))
  (when (root tree)
    (render renderer (root tree)))
  (render renderer (popups tree)))

(defmethod maybe-render ((renderer renderer) (tree layout-tree))
  (when (root tree)
    (maybe-render renderer (root tree)))
  (maybe-render renderer (popups tree)))

(defmethod handle ((event layout-event) (tree layout-tree))
  (or (handle event (popups tree))
      (handle event (root tree))
      (when (typep event 'pointer-move)
        (setf (cursor (ui tree)) NIL))
      (decline)))

(defmethod suggest-size (size (tree layout-tree))
  (let ((root (root tree))
        (popups (popups tree)))
    (suggest-size size root)
    (setf (bounds root) size)
    (setf (bounds popups) size)))

(defmethod refresh ((tree layout-tree))
  (refresh (root tree))
  (refresh (popups tree)))

(defmethod prepare-for-render ((tree layout-tree) (renderer renderer))
  (prepare-for-render (root tree) renderer)
  (prepare-for-render (popups tree) renderer))

(defmethod w ((tree layout-tree)) (w (root tree)))
(defmethod h ((tree layout-tree)) (h (root tree)))
