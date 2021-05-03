#|
 This file is a part of Alloy
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.alloy)

(deftype focus ()
  `(member NIL :weak :strong))

(defgeneric focus-tree (focus-element))
(defgeneric focus-parent (focus-element))
(defgeneric focus (focus-element))
(defgeneric (setf focus) (focus-state focus-element))
(defgeneric exit (focus-element))
(defgeneric activate (focus-element))

(cl:declaim (ftype (function (T) focus) focus))
(cl:declaim (ftype (function (focus T) focus) (setf focus)))

(defgeneric notice-focus (focus-element focus-chain))
(defgeneric index (focus-chain))
(defgeneric (setf index) (index focus-chain))
(defgeneric focused (focus-chain))
(defgeneric (setf focused) (focus-element focus-chain))
(defgeneric focus-next (focus-chain))
(defgeneric focus-prev (focus-chain))
(defgeneric focus-up (focus-grid))
(defgeneric focus-down (focus-grid))

(defgeneric root (focus-tree))

(defclass focus-element (element)
  ((focus-tree :initform NIL :reader focus-tree :writer set-focus-tree)
   (focus-parent :reader focus-parent)
   (focus :initform NIL :accessor focus)))

(defmethod initialize-instance :after ((element focus-element) &key focus-parent focus)
  (when focus-parent
    (enter element focus-parent))
  (when focus (setf (focus element) focus)))

(defmethod print-object ((element focus-element) stream)
  (print-unreadable-object (element stream :type T :identity T)
    (format stream "~s" (focus element))))

(defmethod set-focus-tree :before (tree (element focus-element))
  (when (focus-tree element)
    (when (and tree (not (eq tree (focus-tree element))))
      (error 'element-has-different-root
             :bad-element element :container tree))
    (when (eq (focused (focus-tree element)) element)
      ;; KLUDGE: We have been unhooked. Ideally we'd find the closest, still-hooked parent.
      ;;         however, since currently unhooking happens from the leaf up, we don't know
      ;;         at which parent the hooking would happen, so set root as focus for now.
      (setf (focused (focus-tree element)) NIL))))

(defmethod (setf focus) :around (focus (element focus-element))
  (if (eq focus (focus element))
      focus
      (call-next-method)))

(defmethod (setf focus) :before (focus (element focus-element))
  (check-type focus focus))

(defmethod (setf focus) ((focus (eql :strong)) (element focus-element))
  (setf (focused (focus-tree element)) element)
  (call-next-method)
  (unless (eq element (focus-parent element))
    (loop for child = element then cur
          for cur = (focus-parent element) then (focus-parent cur)
          do (setf (focus cur) :weak)
             (if (eq cur child)
                 (return)
                 (notice-focus child cur))))
  focus)

(defmethod (setf focus) :after ((focus (eql :weak)) (element focus-element))
  (unless (eq element (focus-parent element))
    (notice-focus element (focus-parent element))))

(defmethod activate ((element focus-element))
  (unless (eql :strong (focus element))
    (setf (focus element) :strong))
  element)

(defmethod exit ((element focus-element))
  (when (eql element (focused (focus-tree element)))
    (unless (eql (focus-parent element) element)
      (setf (focus (focus-parent element)) :strong))
    (focus-parent element)))

(defmethod handle :around ((event event) (element focus-element))
  (if (focus-tree element)
      (call-next-method)
      (decline)))

(defmethod handle ((event event) (element focus-element))
  (if (eq element (focus-parent element))
      (decline)
      (or (handle event (focus-parent element))
          (decline))))

(defmethod handle ((event pointer-event) (element focus-element))
  (decline))

(defmethod handle ((event activate) (element focus-element))
  (activate element))

(defmethod handle ((event exit) (element focus-element))
  (exit element))

(defmethod enter :before ((element focus-element) (parent focus-element) &key)
  (cond ((not (slot-boundp element 'focus-parent))
         (set-focus-tree (focus-tree parent) element)
         (setf (slot-value element 'focus-parent) parent))
        ((not (eq parent (focus-parent element)))
         (restart-case
             (error 'element-has-different-parent
                    :bad-element element :container parent :parent (focus-parent element))
           (reparent ()
             :report "Leave the element from its current parent."
             (leave element T))))
        (T
         (error 'element-already-contained
                :bad-element element :container parent))))

(defmethod leave :before ((element focus-element) (parent focus-element))
  (unless (eq parent (focus-parent element))
    (error 'element-has-different-parent
           :bad-element element :container parent :parent (focus-parent element)))
  ;; Make sure we delegate focus to the parent first if we are currently strongly focused
  (when (eq (focused (focus-tree element)) element)
    (exit element)))

(defmethod leave :after ((element focus-element) (parent focus-element))
  (set-focus-tree NIL element)
  (slot-makunbound element 'focus-parent)
  (setf (slot-value element 'focus) NIL))

(defmethod leave ((element focus-element) (parent (eql T)))
  (leave element (focus-parent element)))

(defclass focus-chain (focus-element container)
  ((index :initform NIL :accessor index)
   (focused :initform NIL :accessor focused)))

(defmethod leave :after ((element focus-element) (chain focus-chain))
  (when (eq element (focused chain))
    (setf (slot-value chain 'focused) NIL)
    (cond ((< (or (index chain) 0) (element-count chain))
           (setf (index chain) (index chain)))
          ((< 0 (element-count chain))
           (setf (index chain) (1- (element-count chain))))
          (T
           (setf (index chain) NIL)))))

(defmethod set-focus-tree :before (value (chain focus-chain))
  (do-elements (element chain)
    (set-focus-tree value element)))

(defmethod (setf focused) :before ((none null) (chain focus-chain))
  (setf (slot-value chain 'index) NIL)
  (when (focused chain)
    (setf (focus (focused chain)) NIL)))

(defmethod (setf focused) :before ((element focus-element) (chain focus-chain))
  (unless (eq chain (focus-parent element))
    (error 'element-has-different-parent
           :bad-element element :container chain :parent (focus-parent element)))
  (when (focused chain)
    (let ((focused (focused chain)))
      (setf (slot-value chain 'focused) NIL)
      (setf (focus focused) NIL)))
  (setf (slot-value chain 'index) (element-index element chain)))

(defmethod (setf focused) :after ((element focus-element) (chain focus-chain))
  (when (eql NIL (focus element))
    (setf (focus element) :weak)))

(defmethod (setf index) :before ((index integer) (chain focus-chain))
  (unless (<= 0 index (1- (element-count chain)))
    (error 'index-out-of-range
           :index index :range (list 0 (element-count chain))))
  (when (focused chain)
    (let ((focused (focused chain)))
      (setf (slot-value chain 'focused) NIL)
      (setf (focus focused) NIL)))
  (setf (slot-value chain 'focused) (index-element index chain)))

(defmethod (setf index) :after ((index integer) (chain focus-chain))
  (when (and (focused chain) (eql NIL (focus (focused chain))))
    (setf (focus (focused chain)) :weak)))

(defmethod (setf focus) :after ((focus (eql :strong)) (chain focus-chain))
  (when (focused chain)
    (setf (focus (focused chain)) :weak)))

(defmethod (setf focus) :after ((focus (eql NIL)) (chain focus-chain))
  (when (focused chain)
    (setf (focus (focused chain)) NIL)))

(defmethod notice-focus ((element focus-element) (chain focus-chain))
  (when (and (focused chain)
             (not (eq element (focused chain))))
    (setf (focus (focused chain)) NIL))
  (setf (slot-value chain 'focused) element)
  (setf (slot-value chain 'index) (element-index element chain)))

(defmethod element-index :before ((element focus-element) (chain focus-chain))
  (unless (eq chain (focus-parent element))
    (error 'element-not-contained
           :bad-element element :container chain)))

(defmethod focus-next ((chain focus-chain))
  (unless (= 0 (element-count chain))
    (setf (index chain) (mod (1+ (or (index chain) -1)) (element-count chain)))
    (focused chain)))

(defmethod focus-prev ((chain focus-chain))
  (unless (= 0 (element-count chain))
    (setf (index chain) (mod (1- (or (index chain) 0)) (element-count chain)))
    (focused chain)))

(defmethod focus-up ((chain focus-chain))
  (focus-next chain))

(defmethod focus-down ((chain focus-chain))
  (focus-prev chain))

(defmethod activate :around ((chain focus-chain))
  (if (and (eql :strong (focus chain))
           (focused chain))
      (activate (focused chain))
      (call-next-method)))

(defmethod update :after ((element focus-element) (chain focus-chain) &key index)
  ;; Fixup index position
  (when (and index (focused chain))
    (setf (slot-value chain 'index) (element-index (focused chain) chain))))

(defmethod handle ((event activate) (chain focus-chain))
  (if (and (< 0 (element-count chain))
           (eql :strong (focus chain)))
      (activate chain)
      (decline)))

(defmethod handle ((event exit) (chain focus-chain))
  (if (or (eql :strong (focus chain))
          (and (eql :weak (focus chain))
               (< 0 (element-count chain))))
      (exit chain)
      (decline)))

(defmethod handle ((event focus-next) (chain focus-chain))
  (if (and (< 0 (element-count chain))
           (eql :strong (focus chain)))
      (focus-next chain)
      (decline)))

(defmethod handle ((event focus-prev) (chain focus-chain))
  (if (and (< 0 (element-count chain))
           (eql :strong (focus chain)))
      (focus-prev chain)
      (decline)))

(defmethod handle ((event focus-up) (chain focus-chain))
  (if (and (< 0 (element-count chain))
           (eql :strong (focus chain)))
      (focus-up chain)
      (decline)))

(defmethod handle ((event focus-down) (chain focus-chain))
  (if (and (< 0 (element-count chain))
           (eql :strong (focus chain)))
      (focus-down chain)
      (decline)))

(defclass focus-list (focus-chain vector-container)
  ())

;; FIXME: visually represent which focus chain we're going through by associating it with a layout.
;;        This is a problem because we don't know the focus for a layout. Maybe we should make those
;;        another combined subclass? Would be annoying, though.

(defclass focus-grid (focus-list)
  ((width :initarg :width :initform (arg! :width) :accessor width)))

(defmethod focus-up ((chain focus-chain))
  (let* ((idx (or (index chain) 0))
         (col (mod idx (width chain)))
         (row (1- (floor idx (width chain)))))
    (setf (index chain) (+ col (* row (width chain))))))

(defmethod focus-down ((chain focus-chain))
  (let* ((idx (or (index chain) 0))
         (col (mod idx (width chain)))
         (row (1+ (floor idx (width chain)))))
    (setf (index chain) (+ col (* row (width chain))))))

(defclass focus-stack (focus-chain stack-container)
  ())

(defmethod (setf index) :before ((index cons) (stack focus-stack))
  (destructuring-bind (row . col) index
    (unless (<= 0 row (1- (length (layers stack))))
      (error 'index-out-of-range
             :index index :range (list 0 (length (layers stack)))))
    (unless (<= 0 col (1- (length (aref (layers stack) row))))
      (error 'index-out-of-range
             :index index :range (list 0 (length (aref (layers stack) row)))))
    (when (focused stack)
      (let ((focused (focused stack)))
        (setf (slot-value stack 'focused) NIL)
        (setf (focus focused) NIL)))
    (setf (slot-value stack 'focused) (index-element index stack))))

(defmethod (setf index) :after ((index cons) (stack focus-stack))
  (when (and (focused stack) (eql NIL (focus (focused stack))))
    (setf (focus (focused stack)) :weak)))

(defmethod focus-up ((stack focus-stack))
  (when (null (index stack))
    (setf (index stack) '(0 . 0)))
  (destructuring-bind (row . col) (index stack)
    (when (< 0 row)
      (setf (index stack) (cons (1- row) 0)))
    (focused stack)))

(defmethod focus-down ((stack focus-stack))
  (when (null (index stack))
    (setf (index stack) '(0 . 0)))
  (destructuring-bind (row . col) (index stack)
    (when (< row (1- (length (layers stack))))
      (setf (index stack) (cons (1+ row) 0)))
    (focused stack)))

(defmethod focus-next ((stack focus-stack))
  (when (null (index stack))
    (setf (index stack) '(0 . 0)))
  (destructuring-bind (row . col) (index stack)
    (setf (index stack) (cons row (mod (1+ col) (length (aref (layers stack) row)))))
    (focused stack)))

(defmethod focus-prev ((stack focus-stack))
  (when (null (index stack))
    (setf (index stack) '(0 . 0)))
  (destructuring-bind (row . col) (index stack)
    (setf (index stack) (cons row (mod (1- col) (length (aref (layers stack) row)))))
    (focused stack)))

(defclass focus-tree ()
  ((root :initform NIL :accessor root)
   (popups :initform (make-instance 'focus-list) :reader popups)
   (focused :initform NIL :accessor focused)))

(defmethod initialize-instance :after ((tree focus-tree) &key)
  (set-focus-tree tree (popups tree))
  (setf (slot-value (popups tree) 'focus-parent) (popups tree)))

(defmethod enter ((element focus-element) (tree focus-tree) &key force)
  (when (next-method-p) (call-next-method))
  (when force (setf (root tree) NIL))
  (setf (root tree) element))

(defmethod (setf root) :before ((none null) (tree focus-tree))
  (when (root tree)
    (set-focus-tree NIL (root tree))
    (setf (focused tree) NIL)))

(defmethod (setf root) :before ((element focus-element) (tree focus-tree))
  (when (root tree)
    (error 'root-already-established
           :bad-element element :tree tree)))

(defmethod (setf root) :after ((element focus-element) (tree focus-tree))
  (set-focus-tree tree element)
  (setf (slot-value element 'focus-parent) element)
  (setf (focused tree) element))

(defmethod (setf focused) :around ((element focus-element) (tree focus-tree))
  (if (eq element (focused tree))
      element
      (call-next-method)))

(defmethod (setf focused) :before ((element focus-element) (tree focus-tree))
  (unless (eq (focus-tree element) tree)
    (error 'element-has-different-root
           :bad-element element :container tree))
  ;; First unwind all focus from current back to root...
  (when (focused tree)
    (loop for cur = (focused tree) then (focus-parent cur)
          until (eq cur (focus-parent cur))
          do (setf (focus cur) NIL))))

(defmethod (setf focused) :after ((element focus-element) (tree focus-tree))
  (unless (eq :strong (focus element))
    (setf (focus element) :strong)))

(defmethod (setf focused) ((none null) (tree focus-tree))
  (if (root tree)
      (setf (focused tree) (root tree))
      (call-next-method)))

(defmethod handle ((event event) (tree focus-tree))
  (or (handle event (popups tree))
      (when (focused tree)
        (handle event (focused tree)))
      (decline)))
