#|
 This file is a part of Alloy
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.alloy)

(defclass window (structure)
  ((state :initform :normal :accessor state)
   (central-layout :accessor central-layout)
   (normal-bounds :initform (extent) :accessor normal-bounds)))

(defclass frame (border-layout popup)
  ())

(defmethod handle ((event pointer-event) (layout frame))
  (restart-case (call-next-method)
    (decline ())))

(defclass window-title (label* draggable)
  ())

(define-observable close (observable))
(define-observable minimize (observable))
(define-observable maximize (observable))

(defmethod close ((structure window))
  (leave structure T))

(defmethod restore ((structure window))
  (case (state structure)
    (:minimized
     (enter (central-layout structure) (layout-element structure))
     (setf (bounds structure)
           (px-extent (pxx structure)
                      (+ (- (pxy structure) (pxh (normal-bounds structure)))
                         (pxh structure))
                      (w (normal-bounds structure))
                      (h (normal-bounds structure)))))
    (:maximized
     (setf (bounds (layout-element structure)) (normal-bounds structure))))
  (setf (state structure) :normal))

(defmethod minimize ((structure window))
  (ecase (state structure)
    (:normal
     (setf (normal-bounds structure) (bounds structure))
     (leave (central-layout structure) (layout-element structure))
     (setf (bounds structure) (bounds (index-element :north (layout-element structure))))
     (setf (state structure) :minimized))
    (:minimized)
    (:maximized (restore structure) (minimize structure))))

(defmethod maximize ((structure window))
  (ecase (state structure)
    (:normal
     (setf (normal-bounds structure) (bounds structure))
     (setf (bounds structure) (bounds (layout-parent (layout-element structure))))
     (setf (state structure) :maximized))
    (:minimized (restore structure) (maximize structure))
    (:maximized)))

(defmethod enter ((element layout-element) (structure window) &key)
  (when (next-method-p) (call-next-method))
  (enter element (layout-element structure) :place :center)
  (setf (central-layout structure) element))

(defmethod enter ((element focus-element) (structure window) &key)
  (when (next-method-p) (call-next-method))
  (enter element (focus-element structure) :layer 1))

(defun ensure-centered-extent (thing ui)
  (with-unit-parent (root (layout-tree ui))
    (etypecase thing
      (extent
       thing)
      (margins
       (px-extent
        (pxl thing) (pxb thing)
        (- (pxw (bounds ui)) (pxr thing) (pxl thing))
        (- (pxh (bounds ui)) (pxu thing) (pxb thing))))
      (point
       (px-extent (pxx thing) (pxy thing) 300 200))
      (size
       (px-extent (/ (- (pxw (bounds ui)) (pxw thing)) 2)
                  (/ (- (pxh (bounds ui)) (pxh thing)) 2)
                  (pxw thing)
                  (pxh thing))))))

(defmethod initialize-instance :after ((structure window) &key layout focus (title "Untitled") (closeable T) (minimizable T) (maximizable T) (resizable T) (extent (size 300 200)) (ui (arg! :ui)))
  (let ((frame (make-instance 'frame :padding (if resizable (margins 1 10 1 1) (margins 10))))
        (header (make-instance 'grid-layout :row-sizes '(20) :col-sizes '(T  20 20 20) :cell-margins (margins 2)))
        (focus-stack (make-instance 'focus-stack))
        (title (make-instance 'window-title :value (or title ""))))
    (enter focus-stack (popups (focus-tree ui)))
    (enter frame (popups (layout-tree ui)) :extent (ensure-centered-extent extent ui))
    (enter header frame :place :north)
    (enter title focus-stack :layer 0)
    (enter title header :row 0 :col 0)
    (on drag-to (location title)
      (let ((ox (- (pxx title) (pxx frame)))
            (oy (- (pxy title) (pxy frame))))
        (setf (bounds frame)
              (px-extent (- (pxx location) ox)
                         (- (pxy location) oy)
                         (w frame)
                         (h frame)))
        (notice-size frame (layout-parent frame))))
    (when closeable
      (let ((button (represent "X" 'button)))
        (enter button header :row 0 :col 3)
        (enter button focus-stack :layer 0)
        (on activate (button) (close structure))))
    (when minimizable
      (let ((button (represent "_" 'button)))
        (enter button header :row 0 :col 2)
        (enter button focus-stack :layer 0)
        (on activate (button)
          (case (state structure)
            (:minimized (restore structure))
            (T (minimize structure))))))
    (when maximizable
      (let ((button (represent "o" 'button)))
        (enter button header :row 0 :col 1)
        (enter button focus-stack :layer 0)
        (on activate (button)
          (case (state structure)
            (:maximized (restore structure))
            (T (maximize structure))))))
    (when resizable
      (dolist (side '(:east :south :west))
        (let ((south (make-instance 'resizer :side side :data frame)))
          (enter south frame :place side :size (un 10))
          (enter south focus-stack :layer 2))))
    (finish-structure structure frame focus-stack)
    (when layout
      (enter layout structure))
    (when focus
      (enter focus structure))))

;; FIXME: reinitialize-instance
