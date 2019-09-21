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
  ((focus-tree :initform NIL :reader focus-tree)
   (focus-parent :initarg :focus-parent :initform (arg! :focus-parent) :reader focus-parent)
   (focus :initform NIL :accessor focus)))

(defmethod initialize-instance :after ((element focus-element) &key focus)
  ;; Tie-up with focus-tree root.
  (etypecase (focus-parent element)
    (focus-tree
     (let ((focus-tree (focus-parent element)))
       (setf (slot-value element 'focus-tree) focus-tree)
       (setf (slot-value element 'focus-parent) element)
       (setf (root focus-tree) element)))
    (focus-element
     (setf (slot-value element 'focus-tree) (focus-tree (focus-parent element)))
     (enter element (focus-parent element)))
    (null
     (slot-makunbound element 'focus-parent)))
  (when focus (setf (focus element) focus)))

(defmethod print-object ((element focus-element) stream)
  (print-unreadable-object (element stream :type T :identity T)
    (format stream "~s" (focus element))))

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

(defmethod handle ((event event) (element focus-element) ui)
  (unless (eq element (focus-parent element))
    (handle event (focus-parent element) ui)))

(defmethod handle ((event activate) (element focus-element) ui)
  (activate element))

(defmethod handle ((event exit) (element focus-element) ui)
  (exit element))

(defmethod enter :before ((element focus-element) (parent focus-element) &key)
  (cond ((not (slot-boundp element 'focus-parent))
         (setf (slot-value element 'focus-tree) (focus-tree parent))
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
  (slot-makunbound element 'focus-tree))

(defclass focus-chain (focus-element vector-container)
  ((index :initform -1 :accessor index)
   (focused :initform NIL :accessor focused)))

(defmethod initialize-instance :after ((element focus-chain) &key elements)
  (when elements
    (adjust-array (elements element) (length elements) :initial-contents elements)))

(defmethod reinitialize-instance :after ((element focus-chain) &key elements)
  (when elements
    (when (find (focused element) elements)
      (exit (focused element))
      (setf (focused element) NIL))
    (adjust-array (elements element) (length elements))
    (replace (elements element) elements)))

(defmethod (setf focused) :before ((none null) (chain focus-chain))
  (setf (slot-value chain 'index) -1)
  (when (focused chain)
    (setf (focus (focused chain)) NIL)))

(defmethod (setf focused) :before ((element focus-element) (chain focus-chain))
  (unless (eq chain (focus-parent element))
    (error 'element-has-different-parent
           :element element :container chain :parent (focus-parent element)))
  (setf (slot-value chain 'index) (position element (elements chain)))
  (when (focused chain)
    (setf (slot-value (focused chain) 'focus) NIL)))

(defmethod (setf focused) :after ((element focus-element) (chain focus-chain))
  (when (eql NIL (focus element))
    (setf (focus element) :weak)))

(defmethod (setf index) :before ((index integer) (chain focus-chain))
  (unless (<= 0 index (1- (length (elements chain))))
    (error 'index-out-of-range
           :index index :range (list 0 (length (elements chain)))))
  (when (focused chain)
    (setf (slot-value (focused chain) 'focus) NIL))
  (setf (slot-value chain 'focused) (aref (elements chain) index)))

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
  (unless (= 0 (length (elements chain)))
    (setf (index chain) (mod (1+ (index chain)) (length (elements chain))))
    (focused chain)))

(defmethod focus-prev ((chain focus-chain))
  (unless (= 0 (length (elements chain)))
    (setf (index chain) (mod (1- (index chain)) (length (elements chain))))
    (focused chain)))

(defmethod activate :around ((chain focus-chain))
  (if (and (eql :strong (focus chain))
           (focused chain))
      (activate (focused chain))
      (call-next-method)))

(defmethod update :after ((element focus-element) (chain focus-chain) &key index)
  ;; Fixup index position
  (when (and index (focused chain))
    (setf (slot-value chain 'index) (position (focused chain) (elements chain)))))

(defmethod handle ((event focus-next) (chain focus-chain) ui)
  (focus-next chain))

(defmethod handle ((event focus-prev) (chain focus-chain) ui)
  (focus-prev chain))

(defclass focus-list (focus-chain)
  ())

(defclass focus-grid (focus-chain)
  ((width :initarg :width :initform (arg! :width) :accessor width)))

(defmethod focus-up ((chain focus-chain))
  (let ((col (mod (index chain) (width chain)))
        (row (1- (floor (index chain) (width chain)))))
    (setf (index chain) (+ col (* row (width chain))))))

(defmethod focus-down ((chain focus-chain))
  (let ((col (mod (index chain) (width chain)))
        (row (1+ (floor (index chain) (width chain)))))
    (setf (index chain) (+ col (* row (width chain))))))

(defmethod handle ((event focus-up) (grid focus-grid) ui)
  (focus-up grid))

(defmethod handle ((event focus-down) (grid focus-grid) ui)
  (focus-down grid))

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

(defmethod handle ((event event) (tree focus-tree) ui)
  (handle event (focused tree) ui))

;;; NOTE: Initialisation of the tree must happen roughly as follows:
;;;         (let ((tree (make-instance 'focus-tree))
;;;               (element (make-instance 'focus-element :focus-parent tree)))
;;;           ...)
