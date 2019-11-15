#|
 This file is a part of Alloy
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.alloy)

(defgeneric focus-tree (focus-element))
(defgeneric focus-parent (focus-element))
(defgeneric focus (focus-element))
(defgeneric (setf focus) (focus-state focus-element))
(defgeneric exit (focus-element))
(defgeneric activate (focus-element))
(defgeneric notice-focus (focused parent))

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
   (focus-parent :initarg :focus-parent :reader focus-parent)
   (focus :initform NIL :accessor focus)))

(defmethod initialize-instance :after ((element focus-element) &key focus)
  ;; Tie-up with focus-tree root.
  (when (slot-boundp element 'focus-parent)
    (etypecase (focus-parent element)
      (focus-tree
       (let ((focus-tree (focus-parent element)))
         (set-focus-tree focus-tree element)
         (setf (slot-value element 'focus-parent) element)
         (setf (root focus-tree) element)))
      (focus-element
       (setf (slot-value element 'focus-tree) (focus-tree (focus-parent element)))
       (enter element (focus-parent element)))
      (null
       (slot-makunbound element 'focus-parent))))
  (when focus (setf (focus element) focus)))

(defmethod print-object ((element focus-element) stream)
  (print-unreadable-object (element stream :type T :identity T)
    (format stream "~s" (focus element))))

(defmethod set-focus-tree :before (tree (element focus-element))
  (when (and (focus-tree element) tree (not (eq tree (focus-tree element))))
    (error 'element-has-different-root
           :element element :container tree))
  (when (eq :strong (focus element))
    ;; We have been unhooked. Ideally we'd find the closest, still-hooked parent.
    ;; however, since currently unhooking happens from the leaf up, we don't know
    ;; at which parent the hooking would happen, so
    ;; KLUDGE: set root as focus
    (setf (focused (focus-tree element)) NIL)))

(defmethod (setf focus) :before (focus (element focus-element))
  (check-type focus (member NIL :weak :strong)))

(defmethod (setf focus) :after ((focus (eql :strong)) (element focus-element))
  (setf (focused (focus-tree element)) element))

(defmethod (setf focus) :after (focus (element focus-element))
  (unless (eq element (focus-parent element))
    (notice-focus element (focus-parent element))))

(defmethod activate ((element focus-element))
  (unless (eql :strong (focus element))
    (setf (focus element) :strong))
  element)

(defmethod exit ((element focus-element))
  (unless (eql NIL (focus element))
    (setf (focus element) NIL)
    (setf (focus (focus-parent element)) :strong)
    (focus-parent element)))

(defmethod handle ((event event) (element focus-element))
  (unless (eq element (focus-parent element))
    (handle event (focus-parent element))))

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
         (error 'element-has-different-parent
                :element element :container parent :parent (focus-parent element)))))

(defmethod leave :before ((element focus-element) (parent focus-element))
  (unless (eq parent (focus-parent element))
    (error 'element-has-different-parent
           :element element :container parent :parent (focus-parent element))))

(defmethod leave :after ((element focus-element) (parent focus-element))
  (slot-makunbound element 'focus-parent)
  (set-focus-tree NIL element))

(defclass focus-chain (focus-element container)
  ((index :initform NIL :accessor index)
   (focused :initform NIL :accessor focused)))

(defmethod leave :after ((element focus-element) (chain focus-chain))
  (when (eq element (focused chain))
    (setf (slot-value chain 'focused) NIL)
    (setf (index chain) NIL)))

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
           :element element :container chain :parent (focus-parent element)))
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
  (when (eql NIL (focus (focused chain)))
    (setf (focus (focused chain)) :weak)))

(defmethod element-index :before ((element focus-element) (chain focus-chain))
  (unless (eq chain (focus-parent element))
    (error 'element-not-contained
           :element element :container chain)))

(defmethod notice-focus ((element focus-element) (chain focus-chain))
  (case (focus element)
    (:strong
     (unless (eq element (focused chain))
       (setf (focused chain) element))
     ;; Propagate all the way down
     (loop until (eq chain (focus-parent chain))
           do (setf (focus chain) :weak)
              (setf chain (focus-parent chain))))
    (:weak
     (unless (eq element (focused chain))
       (setf (focused chain) element)))
    ((NIL)
     (when (eq element (focused chain))
       (setf (focus element) :weak)))))

(defmethod focus-next ((chain focus-chain))
  (unless (= 0 (element-count chain))
    (setf (index chain) (mod (1+ (index chain)) (element-count chain)))
    (focused chain)))

(defmethod focus-prev ((chain focus-chain))
  (unless (= 0 (element-count chain))
    (setf (index chain) (mod (1- (index chain)) (element-count chain)))
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

(defmethod handle ((event focus-next) (chain focus-chain))
  (focus-next chain))

(defmethod handle ((event focus-prev) (chain focus-chain))
  (focus-prev chain))

(defmethod handle ((event focus-up) (chain focus-chain))
  (focus-up chain))

(defmethod handle ((event focus-down) (chain focus-chain))
  (focus-down chain))

(defclass focus-list (focus-chain vector-container)
  ())

(defmethod initialize-instance :after ((chain focus-chain) &key elements)
  (when elements
    (adjust-array (elements chain) (length elements) :initial-contents elements)))

(defmethod reinitialize-instance :after ((chain focus-chain) &key elements)
  (when elements
    (when (find (focused chain) elements)
      (exit (focused chain))
      (setf (focused chain) NIL))
    (adjust-array (elements chain) (length elements))
    (replace (elements chain) elements)))

;; FIXME: visually represent which focus chain we're going through by associating it with a layout.
;;        This is a problem because we don't know the focus for a layout. Maybe we should make those
;;        another combined subclass? Would be annoying, though.

(defclass focus-grid (focus-list)
  ((width :initarg :width :initform (arg! :width) :accessor width)))

(defmethod focus-up ((chain focus-chain))
  (let ((col (mod (index chain) (width chain)))
        (row (1- (floor (index chain) (width chain)))))
    (setf (index chain) (+ col (* row (width chain))))))

(defmethod focus-down ((chain focus-chain))
  (let ((col (mod (index chain) (width chain)))
        (row (1+ (floor (index chain) (width chain)))))
    (setf (index chain) (+ col (* row (width chain))))))

(defclass focus-stack (focus-chain stack-container)
  ())

(defmethod focus-up ((stack focus-stack))
  (when (< 0 (car (index stack)))
    (decf (car (index stack)))))

(defmethod focus-down ((stack focus-stack))
  (when (< (car (index stack)) (1- (length (layers stack))))
    (incf (car (index stack)))))

(defmethod focus-next ((stack focus-stack))
  (destructuring-bind (row . col) (index stack)
    (setf (cdr (index stack)) (mod (1+ col) (length (aref (layers stack) row))))))

(defmethod focus-prev ((stack focus-stack))
  (destructuring-bind (row . col) (index stack)
    (setf (cdr (index stack)) (mod (1- col) (length (aref (layers stack) row))))))

(defclass focus-tree ()
  ((root :initform NIL :accessor root)
   (focused :initform NIL :accessor focused)))

(defmethod (setf root) :before ((element focus-element) (tree focus-tree))
  (when (root tree)
    (error 'root-already-established
           :element element :tree tree)))

(defmethod (setf root) :after ((element focus-element) (tree focus-tree))
  (setf (focused tree) element))

(defmethod (setf focused) :around ((element focus-element) (tree focus-tree))
  (unless (eq element (focused tree))
    (call-next-method)))

(defmethod (setf focused) :before ((element focus-element) (tree focus-tree))
  (unless (eq (focus-tree element) tree)
    (error 'element-has-different-root
           :element element :container tree))
  (when (focused tree)
    (setf (focus (focused tree)) NIL)))

(defmethod (setf focused) :after ((element focus-element) (tree focus-tree))
  (unless (eq :strong (focus element))
    (setf (focus element) :strong)))

(defmethod (setf focused) ((none null) (tree focus-tree))
  (if (root tree)
      (setf (focused tree) (root tree))
      (call-next-method)))

(defmethod handle ((event event) (tree focus-tree))
  (unless (handle event (focused tree))
    (decline)))

;;; NOTE: Initialisation of the tree must happen roughly as follows:
;;;         (let ((tree (make-instance 'focus-tree))
;;;               (element (make-instance 'focus-element :focus-parent tree)))
;;;           ...)
