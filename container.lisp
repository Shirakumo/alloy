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
  ((elements :initform (make-array 0 :adjustable T :fill-pointer T) :reader elements)))

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
