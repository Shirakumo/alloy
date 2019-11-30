#|
 This file is a part of Alloy
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.alloy)

(defclass element ()
  ())

(defclass container ()
  ())

(defgeneric enter (element container &key &allow-other-keys))
(defgeneric leave (element container))
(defgeneric update (element container &key &allow-other-keys))
(defgeneric element-count (container))
(defgeneric elements (container))
(defgeneric element-index (element container))
(defgeneric index-element (index container))
(defgeneric call-with-elements (function container &key start end))
(defgeneric clear (container))

(defmacro do-elements ((element container &key start end result) &body body)
  (let ((thunk (gensym "THUNK")))
    `(block NIL
       (flet ((,thunk (,element)
                ,@body))
         (call-with-elements #',thunk ,container :start ,start :end ,end)
         ,result))))

(defmethod shared-initialize :around ((container container) slots &key (elements NIL c-p))
  (call-next-method)
  (when c-p
    (clear container)
    (map NIL (lambda (e) (enter e container)) elements)))

(defmethod describe-object :after ((container container) stream)
  (format stream "~&~%Container Tree:~%")
  (let ((*level* 0))
    (declare (special *level*))
    (labels ((traverse (thing)
               (format stream "~v{ ~}~a~%" (* *level* 2) '(0) thing)
               (when (typep thing 'container)
                 (let ((*level* (1+ *level*)))
                   (declare (special *level*))
                   (do-elements (element thing)
                     (traverse element))))))
      (traverse container))))

(defmethod element-count ((container container))
  (length (elements container)))

(defmethod elements ((container container))
  (let ((list ()))
    (do-elements (element container :result (nreverse list))
      (push element list))))

(defmethod clear ((container container))
  (do-elements (element container)
    (leave element container)))

(defclass vector-container (container)
  ((elements :initform (make-array 0 :adjustable T :fill-pointer T :initial-element NIL) :reader elements)))

(defmethod enter ((element element) (container vector-container) &key index)
  (if index
      (array-utils:vector-push-extend-position element (elements container) index)
      (vector-push-extend element (elements container)))
  element)

(defmethod leave ((element element) (container vector-container))
  (array-utils:vector-pop-position (elements container) (position element (elements container)))
  element)

(defmethod update ((element element) (container vector-container) &key index)
  (when index
    (let ((pos (position element (elements container))))
      (array-utils:vector-pop-position (elements container) pos)
      (array-utils:vector-push-extend-position element (elements container) index)))
  element)

(defmethod call-with-elements (function (container vector-container) &key start end)
  (loop with elements = (elements container)
        for i from (or start 0) below (or end (length elements))
        for element = (aref elements i)
        do (funcall function element)))

(defmethod element-index ((element element) (container vector-container))
  (position element (elements container)))

(defmethod index-element ((index integer) (container vector-container))
  (aref (elements container) index))

(defmethod clear ((container vector-container))
  (loop for i downfrom (1- (length (elements container))) to 0
        do (leave (aref (elements container) i) container)))

(defclass stack-container (container)
  ((layers :initform (make-array 0 :adjustable T :fill-pointer T) :reader layers)))

(defmethod enter ((element element) (container stack-container) &key (layer (max 0 (1- (length (layers container))))))
  (let ((layers (layers container)))
    (when (<= (length layers) layer)
      (adjust-array layers (1+ layer) :initial-element NIL :fill-pointer (1+ layer))
      (loop for i from 0 to layer
            do (unless (aref layers i)
                 (setf (aref layers i) (make-array 0 :adjustable T :fill-pointer T)))))
    (vector-push-extend element (aref layers layer))))

(defmethod update ((element element) (container stack-container) &key layer)
  (when layer
    (let ((layers (layers container)))
      (loop for layer across layers
            for position = (position element layer)
            do (when position
                 (array-utils:vector-pop-position layer position)
                 (return)))
      (when (<= (length layers) layer)
        (adjust-array layers (1+ layer) :initial-element NIL :fill-pointer (1+ layer))
        (loop for i from 0 to layer
              do (unless (aref layers i)
                   (setf (aref layers i) (make-array 0 :adjustable T :fill-pointer T)))))
      (vector-push-extend element (aref layers layer)))))

(defmethod leave ((element element) (container stack-container))
  (loop for layer across (layers container)
        for position = (position element layer)
        do (when position
             (array-utils:vector-pop-position layer position)
             (return))))

(defmethod element-count ((container stack-container))
  (loop for layer across (layers container)
        sum (length layer)))

(defmethod elements ((container stack-container))
  (let ((list ()))
    (loop for layer across (layers container)
          do (loop for element across layer
                   do (push element list)))
    (nreverse list)))

(defmethod element-index ((element element) (container stack-container))
  (loop for layer across (layers container)
        for row from 0
        for col = (position element layer)
        do (when col (return (cons row col)))))

(defmethod index-element ((index cons) (container stack-container))
  (unless (<= 0 (car index) (1- (length (layers container))))
    (error 'index-out-of-range :index (car index) :range (list 0 (1- (length (layers container))))))
  (let ((layer (aref (layers container) (car index))))
    (unless (<= 0 (cdr index) (1- (length layer)))
      (error 'index-out-of-range :index (cdr index) :range (list 0 (1- (length layer)))))
    (aref layer (cdr index))))

(defmethod call-with-elements (function (container stack-container) &key start end)
  (let ((start (or start 0)))
    (loop with i = 0
          for layer across (layers container)
          while (or (not end) (<= end i))
          do (loop for element across layer
                   while (or (not end) (<= end i))
                   do (when (<= start i)
                        (funcall function element))
                      (incf i)))))

(defmethod clear ((container stack-container))
  (loop for layer across (layers container)
        do (loop for i downfrom (1- (length layer)) to 0
                 do (leave (aref layer i) container))))

(defclass single-container (container)
  ((inner :initarg :inner :initform NIL :accessor inner)))

(defmethod enter ((element element) (container single-container) &key)
  (when (inner container)
    (cerror "Replace the element" 'place-already-occupied
            :element element :place T :layout container :existing (inner container)))
  (setf (inner container) element))

(defmethod update ((element element) (container single-container) &key))

(defmethod leave ((element element) (container single-container))
  (setf (inner layout) NIL))

(defmethod call-with-elements (function (container single-container) &key start end)
  (declare (ignore start end))
  (when (inner layout)
    (funcall function (inner layout))))

(defmethod element-count ((container single-container))
  (if (inner container) 1 0))

(defmethod elements ((container single-container))
  (when (inner container) (list (inner container))))

(defmethod element-index ((element element) (container single-container))
  (when (eq element (inner container)) 0))

(defmethod index-element ((index integer) (container single-container))
  (if (and (inner container) (= 0 index))
      (inner container)
      (error 'index-out-of-range :index index :range (list 0 (if (inner container) 1 0)))))

(defmethod clear ((container single-container))
  (setf (inner container) 0))
