#|
 This file is a part of Alloy
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.alloy.test)

(define-test geometry
  :parent alloy)

(define-test point
  :parent geometry
  (of-type alloy:point (alloy:point))
  (is = 0.0f0 (alloy:point-x (alloy:point)))
  (is = 1.0f0 (alloy:point-x (alloy:point 1.0f0)))
  (is = 1.0f0 (alloy:point-x (alloy:point 1)))
  (is = 1.0f0 (alloy:point-x (alloy:point 1.0d0)))
  (is = 1.0f0 (alloy:x (alloy:point 1.0)))
  (is alloy:point= (alloy:point 1 2) (alloy:point 1 2))
  (isnt alloy:point= (alloy:point) (alloy:point 1)))

(define-test extent
  :parent geometry
  (of-type alloy:extent (alloy:extent))
  (is = 0.0f0 (alloy:extent-x (alloy:extent)))
  (is = 1.0f0 (alloy:extent-x (alloy:extent 1.0f0)))
  (is = 1.0f0 (alloy:extent-x (alloy:extent 1)))
  (is = 1.0f0 (alloy:extent-x (alloy:extent 1.0d0)))
  (is = 1.0f0 (alloy:x (alloy:extent 1.0f0)))
  (alloy:destructure-extent (:x x :y y :w w :h h) (alloy:extent 1 2 3 4)
    (is = 1.0f0 x)
    (is = 2.0f0 y)
    (is = 3.0f0 w)
    (is = 4.0f0 h))
  (let ((extent (alloy:extent)))
    (alloy:destructure-extent (:x x) extent
      (setf x 1)
      (is = 0.0f0 (alloy:extent-x extent)))))

(define-test contain-points
  :parent geometry
  :depends-on (point extent)
  (true (alloy:contained-p (alloy:point 5 5) (alloy:extent 0 0 10 10)))
  (true (alloy:contained-p (alloy:point 0 0) (alloy:extent 0 0 10 10)))
  (true (alloy:contained-p (alloy:point 0 10) (alloy:extent 0 0 10 10)))
  (true (alloy:contained-p (alloy:point 10 0) (alloy:extent 0 0 10 10)))
  (true (alloy:contained-p (alloy:point 10 10) (alloy:extent 0 0 10 10)))
  (false (alloy:contained-p (alloy:point 0 15) (alloy:extent 0 0 10 10)))
  (false (alloy:contained-p (alloy:point 15 0) (alloy:extent 0 0 10 10)))
  (false (alloy:contained-p (alloy:point 15 15) (alloy:extent 0 0 10 10)))
  (false (alloy:contained-p (alloy:point -5 -5) (alloy:extent 0 0 10 10))))

(define-test contain-extents
  :parent geometry
  :depends-on (extent)
  (true (alloy:contained-p (alloy:extent 1 1 1 1) (alloy:extent 0 0 3 3)))
  (true (alloy:contained-p (alloy:extent 0 0 1 1) (alloy:extent 0 0 1 1)))
  (true (alloy:contained-p (alloy:extent 0 0 1 1) (alloy:extent -1 -1 2 2)))
  (false (alloy:contained-p (alloy:extent 0 0 2 1) (alloy:extent 0 0 1 1)))
  (false (alloy:contained-p (alloy:extent 0 0 1 2) (alloy:extent 0 0 1 1)))
  (false (alloy:contained-p (alloy:extent 1 1 1 1) (alloy:extent 0 0 1 1))))
