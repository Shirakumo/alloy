#|
 This file is a part of Alloy
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.alloy)

(defclass menubar (horizontal-linear-layout focus-list)
  ((cell-margins :initform (margins 0))))

(defclass submenu (vertical-linear-layout focus-list)
  ((cell-margins :initform (margins 0))))

(defclass menu-item (button* single-container)
  ((on-activate :initform NIL)))

(defmethod set-focus-tree :before (value (item menu-item))
  (when (inner item)
    (set-focus-tree value (inner item))))

(defmethod set-layout-tree :before (value (item menu-item))
  (when (inner item)
    (set-layout-tree value (inner item))))

(defmethod notice-focus (sub (item menu-item)))
(defmethod notice-bounds (sub (item menu-item)))

(defmethod enter :around ((child menu-item) (parent menu-item) &key)
  (enter child (inner parent)))

(defmethod activate ((item menu-item))
  (let ((submenu (inner item)))
    (cond (submenu
           (with-unit-parent item
             (let* ((extent (bounds item))
                    (bounds (suggest-bounds (px-extent (+ (pxx extent) (if (typep (layout-parent item) 'menubar)
                                                                           0 (pxw extent)))
                                                       (+ (pxy extent) (if (typep (layout-parent item) 'menubar)
                                                                           0 (pxh extent)))
                                                       (pxw extent) (pxh extent))
                                            submenu)))
               (setf (bounds submenu) (px-extent (pxx bounds)
                                                 (- (pxy bounds) (pxh bounds))
                                                 (pxw bounds) (pxh bounds)))
               (setf (focus submenu) :strong))))
          (T
           (funcall (on-activate item))))))

(defmethod render :after ((renderer renderer) (item menu-item))
  (when (and (inner item) (focus (inner item)))
    (render renderer (inner item))))

(defclass menu (structure)
  ())

(defmethod initialize-instance :after ((structure menu) &key tree focus-parent layout-parent)
  (let ((layout (make-instance 'menubar :layout-parent layout-parent :focus-parent focus-parent)))
    (labels ((recurse (items parent)
               (loop for (label contents) in items
                     for button = (make-instance 'menu-item :value label)
                     do (etypecase contents
                          (list
                           (recurse contents (make-instance 'submenu :layout-parent button)))
                          ((or symbol function)
                           (setf (on-activate button) contents)))
                        (enter button parent))))
      (recurse tree layout))
    (finish-structure structure layout layout)))

(defmacro with-menu (&body items)
  (labels ((compile-item (item)
             (destructuring-bind (name . items) item
               `(list ,name
                      ,(cond ((null items)
                              '(constantly T))
                             ((and (listp (car items)) (stringp (caar items)))
                              `(list ,@(mapcar #'compile-item items)))
                             ((and (listp (car items)) (eql 'function (car items)))
                              (first items))
                             (T
                              `(lambda () ,@items)))))))
    `(make-instance 'menu :tree (list ,@(mapcar #'compile-item items)))))
