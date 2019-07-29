#|
 This file is a part of Alloy
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.alloy.test)

(define-test focus-tree
  :parent alloy)

(define-test focus-root
  :parent focus-tree
  (let ((tree (make-instance 'alloy:focus-tree)))
    (is eq NIL (alloy:root tree))
    (is eq NIL (alloy:focused tree))
    (let ((el (make-instance 'alloy:focus-element :parent tree)))
      (is eq el (alloy:root tree))
      (is eq el (alloy:focused tree))
      (is eq tree (alloy:focus-tree el))
      (is eq el (alloy:parent el))
      (is eq :strong (alloy:focus el)))))

(define-test focus-root-invariant
  :parent focus-tree
  :depends-on (focus-root)
  (let* ((tree (make-instance 'alloy:focus-tree))
         (el (make-instance 'alloy:focus-element :parent tree)))
    (fail (make-instance 'alloy:focus-element :parent tree))
    (is eq el (alloy:exit el))
    (is eq el (alloy:root tree))
    (is eq :strong (alloy:focus el))))

(define-test focus-list
  :parent focus-tree
  :depends-on (focus-root))

(define-test empty-focus-list
  :parent focus-list
  (let* ((tree (make-instance 'alloy:focus-tree))
         (list (make-instance 'alloy:focus-list :parent tree)))
    (is eq NIL (alloy:focused list))
    (is eq NIL (alloy:focus-next list))
    (is eq NIL (alloy:focus-prev list))
    (fail (setf (alloy:index list) 0))
    (is eq list (alloy:activate list))))

(define-test one-level-focus-list
  :parent focus-list
  :depends-on (empty-focus-list vector-container)
  (let* ((tree (make-instance 'alloy:focus-tree))
         (list (make-instance 'alloy:focus-list :parent tree))
         (e1 (make-instance 'alloy:focus-element :parent list))
         (e2 (make-instance 'alloy:focus-element :parent list))
         (e3 (make-instance 'alloy:focus-element :parent list)))
    (is eq list (alloy:parent e1))
    (is eq list (alloy:parent e2))
    (is eq list (alloy:parent e3))
    (is eq NIL (alloy:focus e1))
    (is eq NIL (alloy:focus e2))
    (is eq NIL (alloy:focus e3))
    (is eq NIL (alloy:focused list))
    ;; Weak focus cycling
    (is eq e1 (alloy:focus-next list))
    (is eq e1 (alloy:focused list))
    (is eq :weak (alloy:focus e1))
    (is eq e2 (alloy:focus-next list))
    (is eq e2 (alloy:focused list))
    (is eq :weak (alloy:focus e2))
    (is eq NIL (alloy:focus e1))
    (is eq e3 (alloy:focus-next list))
    (is eq e3 (alloy:focused list))
    (is eq :weak (alloy:focus e3))
    (is eq NIL (alloy:focus e2))
    (is eq NIL (alloy:focus e1))
    (is eq e1 (alloy:focus-next list))
    (is eq e1 (alloy:focused list))
    (is eq :weak (alloy:focus e1))
    (is eq NIL (alloy:focus e3))
    (is eq NIL (alloy:focus e2))
    (is eq list (alloy:focused tree))
    ;; Inner focus passing
    (is eq e1 (alloy:activate list))
    (is eq e1 (alloy:focused list))
    (is eq :strong (alloy:focus e1))
    (is eq e1 (alloy:focused tree))
    (is eq list (alloy:exit e1))
    (is eq :weak (alloy:focus e1))
    (is eq e1 (alloy:focused list))
    (is eq list (alloy:focused tree))
    (is eq :strong (alloy:focus list))
    ;; Focus stealing
    (is eq e1 (alloy:activate list))
    (is eq e3 (alloy:activate e3))
    (is eq :strong (alloy:focus e3))
    (is eq e3 (alloy:focused tree))
    (is eq e3 (alloy:focused list))
    (is eq NIL (alloy:focus e1))
    (is eq list (setf (alloy:focused tree) list))
    (is eq :strong (alloy:focus list))
    (is eq :weak (alloy:focus e3))
    (is eq e3 (alloy:focused list))
    ;; Index checks
    (is = 0 (alloy:element-index e1 list))
    (is = 1 (alloy:element-index e2 list))
    (is = 2 (alloy:element-index e3 list))
    (is = 2 (alloy:index list))
    ;; Index focus stealing
    (is = 0 (setf (alloy:index list) 0))
    (is eq e1 (alloy:focused list))
    (is eq :weak (alloy:focus e1))
    (is eq NIL (alloy:focus e3))
    (is eq :strong (alloy:focus list))
    (is eq list (alloy:focused tree))
    ;; Index update
    (is eq e1 (alloy:update e1 list :index 1))
    (is = 1 (alloy:element-index e1 list))
    (is = 0 (alloy:element-index e2 list))
    (is = 2 (alloy:element-index e3 list))
    (is = 1 (alloy:index list))
    (is eq e1 (alloy:focused list))))

(define-test multi-level-focus-list
  :parent focus-list
  :depends-on (one-level-focus-list)
  (let* ((tree (make-instance 'alloy:focus-tree))
         (root (make-instance 'alloy:focus-list :parent tree))
         (e1 (make-instance 'alloy:focus-element :parent root))
         (e2 (make-instance 'alloy:focus-list :parent root))
         (e2e1 (make-instance 'alloy:focus-element :parent e2))
         (e2e2 (make-instance 'alloy:focus-element :parent e2))
         (e3 (make-instance 'alloy:focus-list :parent root))
         (e3e1 (make-instance 'alloy:focus-list :parent e3))
         (e3e1e1 (make-instance 'alloy:focus-element :parent e3e1)))
    (declare (ignore e1))
    ;; Simple focus movement
    (is eq e2 (alloy:activate e2))
    (is eq :strong (alloy:focus e2))
    (is eq e2 (alloy:focused tree))
    (is eq e2 (alloy:focused root))
    (is eq e2e1 (alloy:focus-next e2))
    (is eq e2e1 (alloy:focused e2))
    (is eq e2e1 (alloy:activate e2))
    (is eq e2e1 (alloy:focused tree))
    (is eq e2 (alloy:exit e2e1))
    (is eq :strong (alloy:focus e2))
    (is eq e2 (alloy:focused tree))
    (is eq root (alloy:exit e2))
    (is eq e2e2 (alloy:activate e2e2))
    (is eq e2e2 (alloy:focused tree))
    (is eq e2e2 (alloy:focused e2))
    (is eq e2 (alloy:focused root))
    ;; Deep focus stealing
    (is eq e3e1e1 (alloy:activate e3e1e1))
    (is eq :strong (alloy:focus e3e1e1))
    (is eq e3e1e1 (alloy:focused tree))
    (is eq :weak (alloy:focus e2e2))
    (is eq :weak (alloy:focus e3e1))
    (is eq :weak (alloy:focus e3))
    (is eq e3 (alloy:focused root))
    (is eq e3e1 (alloy:focused e3))
    (is eq e3e1e1 (alloy:focused e3e1))))

(define-test focus-tree-component
  :parent focus-tree
  :depends-on (element-table multi-level-focus-list)
  (let* ((tree (make-instance 'alloy:focus-tree))
         (root (make-instance 'alloy:focus-list :parent tree))
         (c1 (make-instance 'alloy:component))
         (c2 (make-instance 'alloy:component))
         (e1 (make-instance 'alloy:focus-entry :parent root :component c1))
         (e2 (make-instance 'alloy:focus-list :parent root))
         (e3 (make-instance 'alloy:focus-list :parent root))
         (e3e1 (make-instance 'alloy:focus-list :parent e3))
         (e3e1e1 (make-instance 'alloy:focus-entry :parent e3e1 :component c2)))
    (is eq e1 (alloy:focus-element c1 tree))
    (is eq e3e1e1 (alloy:focus-element c2 tree))
    (is eq e1 (alloy:leave e1 root))
    (fail (alloy:focus-element c1 tree))
    (is eq e3e1e1 (alloy:focus-element c2 tree))
    (is eq e1 (alloy:enter e1 root))
    (is eq e1 (alloy:focus-element c1 tree))))
