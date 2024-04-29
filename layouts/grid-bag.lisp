(in-package #:org.shirakumo.alloy)

;;; Structure
;;;
;;;   layout           the layout object
;;;   element          component contained in the layout
;;;   element-info     layout information for one element (1-to-1 correspondence
;;;                    to elements)
;;;   cell-info        placement information for one grid cell. stored in a
;;;                    row x columns array
;;;   {row,col}-sizes  vector of size constraints for each row and each column
;;;                    respectively
;;;
;;; element-info
;;;   x, y          leading edge position as grid cell coordinate
;;;   w, h          size as number of spanned grid cells
;;;   weight-{x,y}  priority when extra space is distributed
;;;   ideal-size    cached ideal size of the associated element
;;;
;;; cell-info
;;;   x, y          position of upper left corner of cell in pixels
;;;   w, h          width and height of cell in pixels
;;    element-info  reference to element-info for occupying element
;;;
;;; Behavior
;;;
;;;   The grid is eagerly adjusted when elements are added and removed. That is,
;;;   the dimensions and elements of the element vector, the element-info vector
;;;   and the cell-info grid are always up-to-date.
;;;
;;;   The component placement, on the other hand, is computed on-demand (after
;;;   adding/removing components, after (setf bounds), etc.) according to the
;;;   following strategy:
;;;   1. Retrieve suggested sizes of all layout elements
;;;      (see GATHER-PREFERRED-SIZES)
;;;   2. Compute minimal sizes and fill/fill weight information for rows and
;;;      columns (see COMPUTE-GRID-LAYOUT)
;;;   3. Distributing remaining horizontal/vertical space to columns and rows
;;;      according to their filling behavior and fill weight.
;;;   4. Assign positions and sizes to layout elements.

(deftype grid-growth-policy ()
  '(member NIL :horizontal :vertical :both))

(deftype grid-axis-position ()
  '(integer 0))

(deftype grid-axis-size ()
  '(integer 1))

(deftype grid-axis-weight ()
  '(real 0))

(defclass grid-bag-layout (grid-constraints-mixin layout vector-container)
  ((growth-policy :initarg :growth-policy :type grid-growth-policy :accessor growth-policy :initform NIL)
   ;; This slot contains a vector of `grid-element-info' instances that is
   ;; "parallel" to the element vector in the sense that the information for an
   ;; element is stored at the same index as the element.
   (element-infos :initform (make-array 0 :adjustable T :fill-pointer T :initial-element NIL)
                  :reader element-infos)
   ;; This slot contains a two-dimensional array of `cell-info' instances each
   ;; of which describes the extends of a grid cell and the element allocated to
   ;; that cell.
   (cells :initform (make-array '(0 0) :adjustable T) :reader cells)))

(defmacro check-grid-options (&key always-present)
  `(progn
     ,@(loop for (name . type) in '((x . grid-axis-position)
                                    (y . grid-axis-position)
                                    (w . grid-axis-size)
                                    (h . grid-axis-size)
                                    (weight-x . grid-axis-weight)
                                    (weight-y . grid-axis-weight))
             for supplied = (intern (format NIL "~a-P" name))
             collect (if (member name always-present)
                         `(check-type ,name ,type)
                         `(when ,supplied
                            (check-type ,name ,type))))))

(defmethod enter :around ((element layout-element) (layout grid-bag-layout)
                          &rest args &key (x NIL x-p) (y NIL y-p) (w 1) (h 1)
                                          (weight-x NIL weight-x-p)
                                          (weight-y NIL weight-y-p))
  ;; This method has two purposes: 1) validating the keyword arguments and
  ;; signaling an error if the requested placement is not possible 2) computing
  ;; a suitable placement if X and Y have not been supplied.
  (when (or (and x-p (not y-p)) (and (not x-p) y-p))
    (error "Must either supply both ~s and ~s or neither at the moment" :x :y))
  (check-grid-options :always-present (w h))
  (let* ((cells (cells layout))
         (growth-policy (growth-policy layout))
         (row-count (length (row-sizes layout)))
         (row-limit (unless (member growth-policy '(:vertical :both))
                      row-count))
         (col-count (length (col-sizes layout)))
         (col-limit (unless (member growth-policy '(:horizontal :both))
                      col-count)))
    ;; If X and Y have not been supplied, try to find a(a) unoccupied cell(s)
    ;; for ELEMENT: if GROWTH-POLICY is :VERTICAL scan the bottom row for
    ;; unoccupied cells, if GROWTH-POLICY is :HORIZONTAL, scan the right column
    ;; for unoccupied cells.
    (flet ((placement-possible-p (x y)
             (%map-element-cells
              (lambda (cell x y)
                (declare (ignore x y))
                (when (or (null cell)                       ; outside grid
                          (not (null (element-info cell)))) ; occupied
                  (return-from placement-possible-p NIL)))
              cells x y w h)
             T))
      (cond (x-p)
            ((member growth-policy '(:vertical :both)) ; prefer downward if :BOTH
             (setf y row-count x 0) ; overwritten below if possible
             (loop with candidate-y = (1- row-count)
                   for candidate-x from 0 below col-count
                   when (placement-possible-p candidate-x candidate-y)
                     do (setf y candidate-y x candidate-x)
                        (loop-finish)))
            ((eq growth-policy :horizontal)
             (setf x col-count y 0) ; overwritten below if possible
             (loop with candidate-x = (1- col-count)
                   for candidate-y from 0 below row-count
                   when (placement-possible-p candidate-x candidate-y)
                     do (setf x candidate-x y candidate-y)
                        (loop-finish)))
            (T
             (error 'layout-cannot-grow :layout layout
                                        :direction :horizonal
                                        :growth-policy growth-policy))))
    ;; Signal an error if the requested placement is not within the current cell
    ;; grid and the cell grid is not allowed to grow.
    (let ((elements (elements layout)))
      (%map-element-cells
       (lambda (cell x y)
         ;; CELL is NIL when the current grid doesn't have a cell at that
         ;; location.
         (when cell
           (let ((old-element-info (element-info cell)))
             (when old-element-info
               (let* ((old-element-index (position old-element-info
                                                   (element-infos layout)))
                      (old-element (aref elements old-element-index)))
                 (error 'place-already-occupied :bad-element element
                                                :place (cons y x)
                                                :layout layout
                                                :existing old-element)))))
         ;; This is not the same as checking whether CELL is null since we need
         ;; to know the problematic axis to check whether that axis allows
         ;; growth.
         (when (or (and row-limit (not (< y row-limit)))
                   (and col-limit (not (< x col-limit))))
           (error 'place-does-not-exist :bad-element element
                                        :place (cons x y)
                                        :layout layout
                                        :dimensions (cons col-limit row-limit)
                                        :growth-policy growth-policy)))
       (cells layout) x y w h))
    (apply #'call-next-method element layout :x x :y y args)))

(defmethod enter ((element layout-element) (layout grid-bag-layout)
                  &rest args &key)
  (let* ((elements (elements layout))
         (element-infos (element-infos layout))
         (element-info (apply #'make-instance 'grid-element-info args)))
    (vector-push-extend element elements)
    (vector-push-extend element-info element-infos)
    (update-grid (cells layout) element-infos layout)
    (notice-size layout T)))

(defmethod leave ((element layout-element) (layout grid-bag-layout))
  (let* ((elements (elements layout))
         (element-infos (element-infos layout))
         (index (position element elements)))
    (array-utils:vector-pop-position elements index)
    (array-utils:vector-pop-position element-infos index)
    (update-grid (cells layout) element-infos layout)
    (notice-size layout T))
  element)

(defmethod update :before ((element layout-element) (layout grid-bag-layout)
                           &key (x NIL x-p) (y NIL y-p) (w NIL w-p) (h NIL h-p)
                                (weight-x NIL weight-x-p)
                                (weight-y NIL weight-y-p))
  (check-grid-options))

(defmethod update ((element layout-element) (layout grid-bag-layout)
                   &key (x NIL x-p) (y NIL y-p) (w NIL w-p) (h NIL h-p)
                        (weight-x NIL weight-x-p) (weight-y NIL weight-y-p))
  (let* ((elements (elements layout))
         (index (position element elements))
         (element-infos (element-infos layout))
         (element-info (aref element-infos index)))
    (macrolet ((update (&rest names)
                 `(progn
                    ,@(loop for name in names
                            for supplied = (intern (format NIL "~a-P" name))
                            collect `(when ,supplied
                                       (setf (slot-value element-info ',name)
                                             ,name))))))
      (update x y w h weight-x weight-y))
    (update-grid (cells layout) element-infos layout))
  element)

(defmethod clear ((layout grid-bag-layout))
  (map NIL (lambda (element) (leave element layout))
       (copy-seq (elements layout))))

(macrolet
    ((define (add-name remove-name reader direction)
       `(progn
          (defmethod ,add-name ((layout grid-bag-layout) (size T))
            (let ((policy (growth-policy layout)))
              (unless (member policy '(,direction :both))
                (error 'layout-cannot-grow :layout layout
                                           :direction ,direction
                                           :growth-policy policy)))
            (vector-push-extend (coerce-grid-size size) (,reader layout))
            (update-grid (cells layout) (element-infos layout) layout)
            (refit layout))

          (defmethod ,remove-name ((layout grid-bag-layout))
            (vector-pop (,reader layout))
            (update-grid (cells layout) (element-infos layout) layout)
            (refit layout)))))
  (define add-row remove-row row-sizes :vertical)
  (define add-col remove-col col-sizes :horizontal))

(defmethod (setf bounds) :after (extent (layout grid-bag-layout))
  (refit layout))

(defmethod notice-size ((target layout-element) (layout grid-bag-layout))
  (refit layout))

(defmethod suggest-size ((size size) (layout grid-bag-layout))
  (let ((elements (elements layout))
        (element-infos (element-infos layout))
        (cells (cells layout)))
    ;; Compute a grid layout with an unknown available size.
    (gather-preferred-sizes element-infos elements layout)
    (compute-grid-layout cells NIL layout)
    (if (plusp (array-total-size cells))
        (let ((max-cell (aref cells
                              (1- (array-dimension cells 0))
                              (1- (array-dimension cells 1)))))
          (px-size (max (+ (x max-cell) (w max-cell)) (to-px (w size)))
                   (max (+ (y max-cell) (h max-cell)) (to-px (h size)))))
        (size))))

(defmethod refit ((layout grid-bag-layout))
  (let ((elements (elements layout))
        (element-infos (element-infos layout))
        (cells (cells layout))
        (bounds (bounds layout)))
    ;; Compute a grid layout with (bounds layout) as the available size.
    (gather-preferred-sizes element-infos elements layout)
    (compute-grid-layout cells bounds layout)
    ;; Assign computed bounds
    (loop for element across elements
          for element-info across element-infos
          ;; SBCL does not cons for this.
          for (min max) = (multiple-value-list
                           (element-boundary-cells element-info cells))
          ;; We performed our layout computations in a
          ;; positive-y-direction-is-downwards coordinate system, so flip y
          ;; coordinates here.
          for w = (- (+ (w max) (x max)) (x min))
          for h = (- (+ (h max) (y max)) (y min))
          for x = (x min)
          for y = (- (pxh bounds) (y min) h)
          do (setf (bounds element) (px-extent x y w h)))))

;;; Internal layout stuff

(defclass rectangle-mixin () ()) ; TODO what to do with this?

(defmethod print-object ((object rectangle-mixin) stream)
  (print-unreadable-object (object stream :type T :identity T)
    (let* ((x1 (x object))
           (y1 (y object))
           (w (w object))
           (h (h object))
           (x2 (+ x1 w))
           (y2 (+ y1 h)))
      (format stream "~d:~d ~d:~d [~dx~d]" x1 y1 x2 y2 w h))))

;;; Layout information associated with a single element of the layout.
(defclass grid-element-info (rectangle-mixin)
  (;; Integer cell coordinates
   (x :initarg :x :type grid-axis-position :reader x)
   (y :initarg :y :type grid-axis-position :reader y)
   (w :initarg :w :type grid-axis-size :reader w :initform 1)
   (h :initarg :h :type grid-axis-size :reader h :initform 1)
   (weight-x :initarg :weight-x :type grid-axis-weight :reader weight-x :initform 0)
   (weight-y :initarg :weight-y :type grid-axis-weight :reader weight-y :initform 0)
   (ideal-size :initarg :ideal-size :accessor ideal-size)))

(defun gather-preferred-sizes (element-infos elements layout)
  (with-unit-parent layout
    (loop for element across elements
          for element-info across element-infos
          for ideal-size = (suggest-size (px-size 0 0) element)
          do (setf (ideal-size element-info) ideal-size))))


(defclass cell-info (rectangle-mixin)
  (;; In pixels
   (x :initarg :x :accessor x)
   (y :initarg :y :accessor y)
   (w :accessor w :initform 0)
   (h :accessor h :initform 0)
   (element-info :accessor element-info :initform NIL)))

(defun element-boundary-cells (element-info cells)
  (let* ((x1 (x element-info))
         (y1 (y element-info))
         (x2 (+ x1 (w element-info) -1))
         (y2 (+ y1 (h element-info) -1)))
    (values (aref cells y1 x1) (aref cells y2 x2))))

(defun %map-element-cells (function cells x y w h)
  (loop with y1 = y
        with y2 = (+ y1 h)
        for y from y1 below y2
        do (loop with x1 = x
                 with x2 = (+ x1 w)
                 for x from x1 below x2
                 for cell = (when (array-in-bounds-p cells y x)
                              (aref cells y x))
                 do (funcall function cell x y))))

(defun map-element-cells (function cells element-info)
  (%map-element-cells
   function cells
   (x element-info) (y element-info) (w element-info) (h element-info)))

(defun update-grid (cells element-infos layout)
  ;; Update the two-dimensional array CELLS so that the grid covers both, the
  ;; grid spanned by ROW-SIZES and COLS-SIZES as well the occupied cells
  ;; according to ELEMENT-INFOS. If the latter implicit grid is larger than the
  ;; former implicit grit, extend COL-SIZES and/or ROW-SIZES to that larger size
  ;; (if allowed).
  ;;
  ;; Origin is at cell index (0, 0) even if all elements use higher indices.
  (let* ((growth-policy (growth-policy layout))
         (col-sizes (col-sizes layout))
         (row-sizes (row-sizes layout))
         (col-count (length col-sizes))
         (row-count (length row-sizes))
         (x most-positive-fixnum)
         (y most-positive-fixnum)
         (w col-count)
         (h row-count))
    ;; Compute size of cell grid and adjust storage array.
    (loop for element-info across element-infos
          for element-x = (x element-info)
          for element-y = (y element-info)
          do (setf x (min x element-x))
             (setf x (min y element-y))
             (setf w (max w (+ element-x (w element-info))))
             (setf h (max h (+ element-y (h element-info)))))
    ;; Extend ROW/COL-SIZES if required and allowed.
    (flet ((maybe-grow (old-count new-count vector direction)
             (cond ((= old-count new-count))
                   ((or (eq growth-policy direction) (eq growth-policy :both))
                    (let ((fill (elt vector (1- (length vector)))))
                      (adjust-array vector new-count :initial-element fill
                                                     :fill-pointer new-count)))
                   (T
                    (error 'layout-cannot-grow :layout layout
                                               :direction direction
                                               :growth-policy growth-policy)))))
      (maybe-grow col-count w col-sizes :horizontal)
      (maybe-grow row-count h row-sizes :vertical))
    ;; Adjust grid and initialize new cells.
    (adjust-array cells (list h w) :initial-element NIL)
    (loop for y from 0 below h
          do (loop for x from 0 below w
                   when (null (aref cells y x))
                   do (setf (aref cells y x)
                            (make-instance 'cell-info :x x :y y))))
    ;; Assign element indices to cells.
    (loop for element-info across element-infos
          do (map-element-cells
              (lambda (cell x y)
                (declare (ignore x y))
                (setf (element-info cell) element-info))
              cells element-info))
    cells))

(defun compute-grid-layout (cells bounds layout)
  ;; Left, top is at cell index (0, 0) even if all elements use higher
  ;; indices.
  (let* ((col-sizes (col-sizes layout))
         (row-sizes (row-sizes layout))
         (w (array-dimension cells 1))
         (h (array-dimension cells 0))
         (total-width 0)
         (total-height 0)
         (total-weight-x 0)
         (total-weight-y 0)
         (column-widths (make-array w :initial-element NIL))
         (row-heights (make-array h :initial-element NIL)))
    (with-unit-parent layout
      (destructure-margins (:l ml :u mu :r mr :b mb :to-px T) (cell-margins layout)
        ;; Compute minimal width/height and filling and weight information for
        ;; columns and rows. For a given row/column:
        ;; * the minimal size is the maximum over the suggested size of all
        ;;   components and the COL/ROW-SIZES entry (unless T)
        ;; * the fill weight is the sum of the component fill weights plus 1 or
        ;;   0 depending on whether the COL/ROW-SIZES entry is T
        (flet ((analyze-size (specs index)
                 (let* ((size (when (array-in-bounds-p specs index)
                                (aref specs index)))
                        (fill-p (eq size T))
                        (effective-size (if fill-p 0 (to-px size)))
                        (fill-weight (if fill-p 1 0)))
                   (values fill-p effective-size fill-weight)))
               (analyze-cell (row column)
                 (let* ((cell (aref cells row column))
                        (element-info (element-info cell)))
                   (cond (element-info
                          (let ((ideal-size (ideal-size element-info)))
                            (values (/ (pxw ideal-size) (w element-info))
                                    (/ (pxh ideal-size) (h element-info))
                                    (weight-x element-info)
                                    (weight-y element-info))))
                         (T
                          (values 0 0 0 0))))))
          (loop for column below w
                for x of-type (real 0) = ml then (+ x min-width mr ml)
                for (fill-p width fill-weight) = (multiple-value-list
                                                  (analyze-size col-sizes column))
                for min-width = (max width
                                     (loop for row below h
                                           for (min-width NIL weight)
                                              = (multiple-value-list
                                                 (analyze-cell row column))
                                           do (incf fill-weight weight)
                                           maximize min-width))
                do (setf (aref column-widths column) (cons min-width fill-weight))
                   (incf total-weight-x fill-weight)
                finally (when min-width ; NIL for empty grid
                          (setf total-width (+ x min-width mr))))
          (loop for row below h
                for y of-type (real 0) = mu then (+ y min-height mb mu)
                for (fill-p height fill-weight) = (multiple-value-list
                                                   (analyze-size row-sizes row))
                for min-height = (max height
                                      (loop for column below w
                                            for (NIL min-height NIL weight)
                                               = (multiple-value-list
                                                  (analyze-cell row column))
                                            do (incf fill-weight weight)
                                            maximize min-height))
                do (setf (aref row-heights row) (cons min-height fill-weight))
                   (incf total-weight-y fill-weight)
                finally (when min-height ; NIL for empty grid
                          (setf total-height (+ y min-height mb)))))
        ;; Distribute remaining space among filling rows and columns according
        ;; to fill weights and assign final positions and sizes to cells. BOUNDS
        ;; is null when this is called from SUGGEST-SIZE.
        (let ((available-width  (if bounds
                                    (max 0 (- (pxw bounds) total-width))
                                    0))
              (available-height (if bounds
                                    (max 0 (- (pxh bounds) total-height))
                                    0)))
          (loop for column below w
                for x = ml then (+ x column-width mr ml)
                for (column-width . fill-weight) = (aref column-widths column)
                when (and (plusp available-width) (plusp total-weight-x))
                  do (incf column-width (* available-width (/ fill-weight
                                                              total-weight-x)))
                do (loop for row below h
                         for cell = (aref cells row column)
                         do (setf (x cell) x
                                  (w cell) column-width)))
          (loop for row below h
                for y = mu then (+ y row-height mb mu)
                for (row-height . fill-weight) = (aref row-heights row)
                when (and (plusp available-height) (plusp total-weight-y))
                  do (incf row-height (* available-height (/ fill-weight
                                                             total-weight-y)))
                do (loop for column below w
                         for cell = (aref cells row column)
                         do (setf (y cell) y
                                  (h cell) row-height))))))
    cells))
