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
