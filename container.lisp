#|
 This file is a part of Alloy
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.alloy)

(defclass container ()
  ())

(defgeneric enter (element container &key &allow-other-keys))
(defgeneric leave (element container))
(defgeneric update (element container &key &allow-other-keys))
(defgeneric call-with-elements (function container))

(defmacro do-elements ((element container &optional result) &body body)
  (let ((thunk (gensym "THUNK")))
    `(flet ((,thunk (,element)
              ,@body))
       (call-with-elements #',thunk ,container)
       ,result)))

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

(defclass element-table ()
  ((component-map :initform (make-hash-table :test 'eq) :reader component-map)))

(defgeneric associate (element component element-table))
(defgeneric disassociate (element component element-table))
(defgeneric associated-element (component element-table))

(defmethod associate (element (component component) (table element-table))
  (setf (gethash component (component-map table)) element))

(defmethod disassociate (element (component component) (table element-table))
  (remhash component (component-map table)))

(defmethod associated-element ((component component) (table element-table))
  (gethash component (component-map table)))
