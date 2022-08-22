#|
 This file is a part of Alloy
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.alloy)

(defclass separator (layout-element renderable)
  ())

(defmethod set-focus-tree (tree (separator separator)))
(defmethod suggest-size (size (separator separator))
  (px-size (pxw size) (un 5)))
(defmethod focus ((separator separator)))
(defmethod (setf focus) (focus (separator separator)))
(defmethod handle ((ev pointer-move) (separator separator)))

(defclass menubar (horizontal-linear-layout focus-list)
  ((cell-margins :initform (margins 2 0))))

(defclass submenu (vertical-linear-layout focus-list)
  ((cell-margins :initform (margins 0))
   (min-size :initform (extent 10 10))))

(defclass menu-item (button* single-container)
  ((on-activate :initform NIL)))

(defmethod set-focus-tree :before (value (item menu-item))
  (when (inner item)
    (set-focus-tree value (inner item))))

(defmethod set-layout-tree :before (value (item menu-item))
  (when (inner item)
    (set-layout-tree value (inner item))))

(defmethod notice-focus (sub (item menu-item)))
(defmethod notice-size (sub (item menu-item)))
(defmethod register :after ((item menu-item) (renderer renderer))
  (when (inner item)
    (register (inner item) renderer)))

(defmethod enter :around ((child menu-item) (parent menu-item) &key)
  (enter child (inner parent)))

(defmethod enter :after ((child submenu) (parent menu-item) &key)
  (setf (slot-value child 'layout-parent) parent))

(defmethod activate ((item menu-item))
  (let ((submenu (inner item)))
    (cond (submenu
           (with-unit-parent item
             (let* ((extent (bounds item))
                    (bounds (suggest-size extent submenu)))
               (setf (bounds submenu) (px-extent (if (typep (layout-parent item) 'menubar)
                                                     0 (pxw extent))
                                                 (- (if (typep (layout-parent item) 'menubar)
                                                        0 (pxh extent))
                                                    (pxh bounds))
                                                 (pxw bounds)
                                                 (pxh bounds)))
               (setf (focus submenu) (if (focus submenu) NIL :strong)))))
          (T
           (funcall (on-activate item))
           (setf (focus (root (focus-tree item))) :strong)))))

(defmethod render :after ((renderer renderer) (item menu-item))
  (when (and (inner item) (focus (inner item)))
    (reset-visibility renderer)
    (render renderer (inner item))))

(defclass menu (structure)
  ())

(defmethod initialize-instance :after ((structure menu) &key tree focus-parent layout-parent)
  (let ((layout (make-instance 'menubar :layout-parent layout-parent :focus-parent focus-parent)))
    (labels ((recurse (items parent)
               (dolist (item items)
                 (etypecase item
                   (layout-element
                    (enter item parent))
                   ((eql :separator)
                    (enter (make-instance 'separator) parent))
                   (list
                    (let ((button (make-instance 'menu-item :value (first item) :layout-parent parent))
                          (contents (second item)))
                      (etypecase contents
                        (layout-element
                         (enter contents (inner button)))
                        (list
                         (recurse contents (make-instance 'submenu :layout-parent button)))
                        ((or symbol function)
                         (setf (on-activate button) contents)))))))))
      (recurse tree layout))
    (finish-structure structure layout layout)))

(defmacro with-menu (&body items)
  (labels ((compile-item (item)
             (if (listp item)
                 (destructuring-bind (name . items) item
                   `(list ,name
                          ,(cond ((null items)
                                  '(constantly T))
                                 ((symbolp (car items))
                                  (car items))
                                 ((stringp (caar items))
                                  `(list ,@(mapcar #'compile-item items)))
                                 ((eql 'function (caar items))
                                  item)
                                 (T
                                  `(lambda () ,@items)))))
                 item)))
    `(make-instance 'menu :tree (list ,@(mapcar #'compile-item items)))))
