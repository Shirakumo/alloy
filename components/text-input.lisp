#|
 This file is a part of Alloy
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.alloy)

(defvar *token-stops* " !?-;.,
")

(defclass cursor ()
  ((pos :initform 0 :reader pos :writer set-pos)
   (anchor :initform NIL :reader anchor :writer set-anchor)
   (component :initarg :component :initform (arg! :component) :reader component)))

(defmethod (setf pos) ((position integer) (cursor cursor))
  (set-pos (max 0 (min position (length (text (component cursor))))) cursor))

(defmethod (setf anchor) ((position integer) (cursor cursor))
  (set-anchor (max 0 (min position (length (text (component cursor))))) cursor))

(defmethod (setf anchor) ((null null) (cursor cursor))
  (set-anchor null cursor))

(defmethod move-to :after (target (cursor cursor))
  (mark-for-render (component cursor)))

(defmethod move-to ((_ (eql :start)) (cursor cursor))
  (set-pos 0 cursor))

(defmethod move-to ((_ (eql :end)) (cursor cursor))
  (set-pos (length (text (component cursor))) cursor))

(defmethod move-to ((_ (eql :prev-char)) (cursor cursor))
  (set-pos (max 0 (1- (pos cursor))) cursor))

(defmethod move-to ((_ (eql :next-char)) (cursor cursor))
  (set-pos (min (length (text (component cursor))) (1+ (pos cursor))) cursor))

(defmethod move-to ((_ (eql :prev-token)) (cursor cursor))
  (let ((string (text (component cursor))))
    (set-pos (loop for i downfrom (1- (pos cursor)) above 0
                   do (when (find (char string i) *token-stops*)
                        (return i))
                   finally (return 0))
             cursor)))

(defmethod move-to ((_ (eql :next-token)) (cursor cursor))
  (let ((string (text (component cursor))))
    (set-pos (loop for i from (1+ (pos cursor)) below (length string)
                   do (when (find (char string i) *token-stops*)
                        (return i))
                   finally (return (length string)))
             cursor)))

(defmethod move-to ((_ (eql :prev-line)) (cursor cursor))
  (let ((pos (pos cursor)))
    (move-to :line-start cursor)
    (when (< 0 (pos cursor))
      (let ((line-end (pos cursor))
            (col (- (pos cursor) pos)))
        (set-pos (1- (pos cursor)) cursor)
        (move-to :line-start cursor)
        (set-pos (min (1- line-end) (+ (pos cursor) col)) cursor)))))

(defmethod move-to ((_ (eql :next-line)) (cursor cursor))
  (let ((pos (pos cursor)))
    (move-to :line-start cursor)
    (let ((col (- (pos cursor) pos)))
      (move-to :line-end cursor)
      (when (< (pos cursor) (length (text (component cursor))))
        (let ((start (1+ (pos cursor))))
          (move-to :line-end cursor)
          (set-pos (min (+ start col) (pos cursor)) cursor))))))

(defmethod move-to ((_ (eql :line-start)) (cursor cursor))
  (let ((string (text (component cursor))))
    (set-pos (loop for i downfrom (pos cursor) above 0
                   do (when (char= #\Linefeed (char string (1- i)))
                        (return i))
                   finally (return 0))
             cursor)))

(defmethod move-to ((_ (eql :line-end)) (cursor cursor))
  (let ((string (text (component cursor))))
    (set-pos (loop for i from (pos cursor) below (length string)
                   do (when (char= #\Linefeed (char string i))
                        (return i))
                   finally (return (length string)))
             cursor)))

(defmethod move-to ((position integer) (cursor cursor))
  (set-pos (max 0 (min position (length (text (component cursor))))) cursor))

(defclass text-input-component (value-component)
  ((insert-mode :initform :add :initarg :insert-mode :accessor insert-mode)
   (placeholder :initform "" :initarg :placeholder :accessor placeholder)
   (cursor :reader cursor)))

(defmethod initialize-instance :after ((component text-input-component) &key)
  (setf (slot-value component 'cursor) (make-instance 'cursor :component component))
  (set-pos (length (text component)) (cursor component)))

(defmethod move-to (place (component text-input-component))
  (move-to place (cursor component)))

(defmethod text ((component text-input-component))
  (value component))

(defmethod (setf text) (text (component text-input-component))
  (check-type text string)
  (setf (value component) text))

(defmethod (setf text) :after (value (component text-input-component))
  ;; Ensure we clamp the cursor.
  (move-to (pos (cursor component)) (cursor component))
  (mark-for-render component))

(defun maybe-enlarge (array size)
  (if (< (array-total-size array) size)
      (adjust-array array size :fill-pointer size)
      (setf (fill-pointer array) size)))

(defmethod insert-text (text (component text-input-component))
  (let ((old (text component))
        (cursor (pos (cursor component))))
    (unless (adjustable-array-p old)
      (setf old (make-array (length old) :element-type 'character :adjustable T :fill-pointer T :initial-contents old)))
    (ecase (insert-mode component)
      (:add
       (let ((to-copy (- (length old) cursor)))
         (maybe-enlarge old (+ (length old) (length text)))
         (loop for i downfrom (1- (fill-pointer old))
               repeat to-copy
               do (setf (aref old i) (aref old (- i (length text))))))
       (replace old text :start1 cursor))
      (:replace
       (maybe-enlarge old (max (length old) (+ cursor (length text))))
       (replace old text :start1 cursor)))
    (setf (text component) old)
    (move-to (+ cursor (length text)) (cursor component))))

(defmethod delete-text (start end (component text-input-component))
  (let ((old (text component))
        (cursor (cursor component)))
    (unless (adjustable-array-p old)
      (setf old (make-array (length old) :element-type 'character :adjustable T :fill-pointer T :initial-contents old)))
    (let* ((start (max 0 (min start (length old))))
           (end (max start (min end (length old)))))
      (when (/= (length old) end)
        (array-utils:array-shift old :n (- start end) :from end :adjust NIL))
      (decf (fill-pointer old) (- end start))
      (when (<= start (pos cursor) end)
        (set-pos start cursor))
      (when (and (anchor cursor) (<= start (anchor cursor) end))
        (set-anchor NIL cursor))
      (setf (text component) old))))

(define-observable accept (observable))
(define-observable reject (observable))

(defmethod accept ((component text-input-component))
  (exit component))

(defmethod reject ((component text-input-component))
  (exit component))

(defmethod handle ((event text-event) (component text-input-component))
  (let ((cursor (cursor component)))
    (when (anchor cursor)
      (delete-text (min (anchor cursor) (pos cursor))
                   (max (anchor cursor) (pos cursor))
                   component)))
  (insert-text (text event) component))

;;; We capture all key-up/down events, as they either are command events
;;; or correspond to text input that we handle in TEXT-EVENT, in which
;;; case we should pretend to have handle them as well.
(defmethod handle ((event key-down) (component text-input-component))
  (let ((cursor (cursor component)))
    (flet ((move (target)
             (cond ((find :shift (modifiers event))
                    (unless (anchor cursor) (set-anchor (pos cursor) cursor)))
                   (T
                    (set-anchor NIL cursor)))
             (move-to target cursor)))
      (case (key event)
        (:backspace
         (cond ((anchor cursor)
                (delete-text (min (anchor cursor) (pos cursor))
                             (max (anchor cursor) (pos cursor))
                             component))
               ((< 0 (pos cursor))
                (delete-text (1- (pos cursor)) (pos cursor) component))))
        (:delete
         (cond ((anchor cursor)
                (delete-text (min (anchor cursor) (pos cursor))
                             (max (anchor cursor) (pos cursor))
                             component))
               ((< (pos cursor) (length (text component)))
                (delete-text (pos cursor) (1+ (pos cursor)) component))))
        (:a
         (when (find :control (modifiers event))
           (set-anchor 0 cursor)
           (move-to :end cursor)))
        (:left
         (if (find :control (modifiers event))
             (move :prev-token)
             (move :prev-char)))
        (:right
         (if (find :control (modifiers event))
             (move :next-token)
             (move :next-char)))
        (:up
         (move :prev-line))
        (:down
         (move :next-line))
        (:home
         (move :start))
        (:end
         (move :end))
        (:insert
         (setf (insert-mode component)
               (ecase (insert-mode component)
                 (:replace :add)
                 (:add :replace))))
        (:escape
         (exit component))
        (T
         (unless (eql :strong (focus component))
           (decline)))))))

(defmethod handle ((event key-up) (component text-input-component))
  ;; FIXME: This is not quite correct as we *do* eat corresponding text inputs...
  (unless (eql :strong (focus component))
    (decline)))

(defmethod handle ((event pointer-up) (component text-input-component))
  ;; TODO: Implement cursor movement via pointer (set cursor, select).
  (if (contained-p (location event) component)
      (setf (focus component) :strong)
      (decline)))

(defmethod handle ((event pointer-down) (component text-input-component)))

;;; TODO: Implement virtual keyboard as a pre-made structure
;;;       with capability to select layout for char mapping

(defmethod handle ((event button-up) (component text-input-component))
  (let ((cursor (cursor component)))
    (flet ((move (target)
             (move-to target cursor)))
      (case (button event)
        (:x )
        (:y (accept component))
        (:b (cond ((anchor cursor)
                   (delete-text (min (anchor cursor) (pos cursor))
                                (max (anchor cursor) (pos cursor))
                                component))
                  ((< 0 (pos cursor))
                   (delete-text (1- (pos cursor)) (pos cursor) component))))
        ;; FIXME: Movement by character should maybe be BIDI sensitive, since
        ;;        we're mapping physical left / right sides to a cursor movement.
        ;;        having the left button advance to the right seems weird.
        ;;        Not sure what the convention is there, though.
        ;;        Note that the same goes for L/R on keyboards.
        (:l1 (move :prev-char))
        (:r1 (move :next-char))
        (:l2 (move :start))
        (:r2 (move :end))
        (T
         (call-next-method))))))

(defmethod handle ((event copy-event) (component text-input-component))
  (let* ((cursor (cursor component))
         (pos (pos cursor))
         (anchor (anchor cursor)))
    (setf (clipboard (ui (layout-tree component)))
          (if anchor
              (subseq (text component) (min pos anchor) (max pos anchor))
              (text component)))))

(defmethod handle ((event cut-event) (component text-input-component))
  (let* ((cursor (cursor component))
         (pos (pos cursor))
         (anchor (anchor cursor)))
    (when anchor
      (setf (clipboard (ui (layout-tree component)))
            (subseq (text component) (min pos anchor) (max pos anchor)))
      (delete-text (min anchor pos) (max anchor pos) component))))

(defmethod handle ((event paste-event) (component text-input-component))
  (let ((content (content event)))
    (typecase content
      (string
       (let ((cursor (cursor component)))
         (when (anchor cursor)
           (delete-text (min (anchor cursor) (pos cursor))
                        (max (anchor cursor) (pos cursor))
                        component)))
       (insert-text content component))
      (T
       (call-next-method)))))

(defclass filtered-text-input (text-input-component)
  ())

(defgeneric accept-character (text-input c &optional state))
(defgeneric filter-text (text-input text))

(defmethod filter-text ((component filtered-text-input) text)
  (let ((state NIL))
    (flet ((process (c)
             (multiple-value-bind (ok next) (apply #'accept-character component c state)
               (setf state (list next))
               ok)))
      ;; Try to preserve identity if possible.
      (if (array-has-fill-pointer-p text)
          (loop with write = 0
                for read from 0 below (length text)
                for include = (process (aref text read))
                do (when include
                     (setf (aref text write) (aref text read))
                     (incf write))
                finally (progn (setf (fill-pointer text) write)
                               (return text)))
          (remove-if-not #'process text)))))

(defmethod (setf text) :around ((text string) (component filtered-text-input))
  (call-next-method (filter-text component text) component))

(defclass validated-text-input (text-input-component)
  ())

(defgeneric valid-p (text-input text))

(defmethod valid-p ((component text-input-component) text)
  T)

(defmethod valid-p ((component validated-text-input) text)
  (let ((state NIL))
    (flet ((process (c)
             (multiple-value-bind (ok next) (apply #'accept-character component c state)
               (setf state (list next))
               ok)))
      (every #'process text))))

(defmethod accept :around ((component validated-text-input))
  (when (valid-p component (text component))
    (call-next-method)))

(defclass transformed-text-input (text-input-component)
  ((text :initform "" :accessor text)
   (previous-value :accessor previous-value)))

(defvar *text-updating-p* NIL)

(defgeneric value->text (text-input value))
(defgeneric text->value (text-input text))

(defmethod value->text ((component transformed-text-input) value)
  (prin1-to-string value))

(defmethod text->value ((component transformed-text-input) text)
  (read-from-string text))

(defmethod initialize-instance :after ((component transformed-text-input) &key)
  (setf (text component) (value->text component (value component))))

(defmethod value-changed :after ((component transformed-text-input))
  (let ((text (value->text component (value component))))
    (when (and (not *text-updating-p*) (string/= text (text component)))
      (setf (text component) text))))

(defmethod (setf focus) :after (value (component transformed-text-input))
  (case value
    (:strong (setf (previous-value component) (value component)))
    ((NIL) (setf (text component) (value->text component (value component))))))

(defmethod (setf text) :after (text (component transformed-text-input))
  (when (valid-p component text)
    (let ((value (text->value component text))
          (*text-updating-p* T))
      (unless (equal value (value component))
        (setf (value component) value)))))

(defmethod accept :after ((component transformed-text-input))
  (slot-makunbound component 'previous-value))

(defmethod reject :before ((component transformed-text-input))
  (when (slot-boundp component 'previous-value)
    (setf (value component) (previous-value component))))

(defclass input-line (text-input-component)
  ())

(defmethod handle ((event key-down) (component input-line))
  (case (key event)
    ((:enter :return)
     (accept component))
    (:escape
     (reject component))
    (T
     (call-next-method))))

(defmethod handle ((event paste-event) (component text-input-component))
  ;; Override to leave out returns and linefeeds.
  (let ((content (content event)))
    (typecase content
      (string
       (insert-text (remove-if (lambda (c) (find c '(#\Return #\Linefeed))) content) component))
      (T
       (call-next-method)))))

(defclass input-box (text-input-component)
  ())

(defmethod handle ((event key-down) (component input-box))
  (case (key event)
    ((:enter :return)
      (if (find :control (modifiers event))
          (accept component)
          (insert-text (string #\Linefeed) component)))
    (:escape
     (reject component))
    (T
     (call-next-method))))
