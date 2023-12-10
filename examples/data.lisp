(in-package #:org.shirakumo.alloy.examples)

(defclass example-object (alloy:observable-object)
  ((foo :initform "foo")
   (bar :initform "bar" :accessor bar-accessor)))

(defclass another-object (alloy:observable-object alloy:remap-data)
  ((far) (boo :accessor boo-accessor)
   (unique :initform "unique")))

(define-example data (screen)
  (let* ((window      (windowing:make-window screen))
	 (object      (make-instance 'example-object))
	 (object-data (make-instance 'alloy:object-data :object object))
         (layout     (make-instance 'alloy:vertical-linear-layout :layout-parent window))
         (focus      (make-instance 'alloy:focus-list :focus-parent window))
	 (refresh    (alloy:represent "Refresh" 'alloy:button :layout-parent layout :focus-parent focus))
	 (adjust-1   (alloy:represent "(slot-value object 'bar)" 'alloy:button))
	 (adjust-2   (alloy:represent "(slot-value object 'foo)" 'alloy:button)))

    (alloy:on alloy:activate (refresh) (alloy:refresh layout))
    (alloy:on alloy:activate (adjust-1) (setf (slot-value object 'bar) (randobar)))
    (alloy:on alloy:activate (adjust-2) (setf (slot-value object 'foo) (randofoo)))

    (enter+ adjust-1 adjust-2 layout)
    (enter+ adjust-1 adjust-2 focus)

    ;; Shows an interaction with the accessor-data class
    (let* ((layout            (make-instance 'alloy:horizontal-linear-layout :layout-parent layout))
	   (bar-accessor-data (make-instance 'alloy:accessor-data :accessor 'bar-accessor :object object))
	   (label             (make-instance 'alloy:label :data bar-accessor-data))
	   (button            (alloy:represent "(alloy:value bar-accessor-data)" 'alloy:button)))
      (alloy:on alloy:activate (button) (setf (alloy:value bar-accessor-data) (randobar)))
      (enter+ button label layout)
      (enter+ button focus))

    ;; Shows an interaction with the slot-data class
    (let* ((layout        (make-instance 'alloy:horizontal-linear-layout :layout-parent layout))
	   (foo-slot-data (make-instance 'alloy:slot-data :slot 'foo :object object))
	   (label         (make-instance 'alloy:label :data foo-slot-data))
	   (button        (alloy:represent "(alloy:value foo-slot-data)" 'alloy:button)))
      (alloy:on alloy:activate (button) (setf (alloy:value foo-slot-data) (randofoo)))
      (enter+ button label layout)
      (enter+ button focus))

    ;; Object-data but label is accessing the bar field of the object
    (let* ((layout (make-instance 'alloy:horizontal-linear-layout :layout-parent layout))
	   (label  (make-instance 'alloy:label :data object-data :value-function 'bar))
	   (button (alloy:represent "(access object-data 'bar)" 'alloy:button)))
      (alloy:on alloy:activate (button) (setf (alloy:access object-data 'bar) (randobar)))
      (enter+ button label layout)
      (enter+ button focus))

    ;; Object-data but label is accessing the foo field of the object
    (let* ((layout (make-instance 'alloy:horizontal-linear-layout :layout-parent layout))
	   (label  (make-instance 'alloy:label :data object-data :value-function 'foo))
	   (button (alloy:represent "(access object-data 'foo)" 'alloy:button)))
      (alloy:on alloy:activate (button) (setf (alloy:access object-data 'foo) (randofoo)))
      (enter+ button label layout)
      (enter+ button focus))

    ;; Attaches observers that will print out the observation
    ;; e.g: (alloy:observe 'bar object-data (lambda (value data) (print "Observation")))
    (loop for (observation observable) in `((foo ,object) (bar ,object) (foo ,object-data) (bar ,object-data))
	  do (let ((observation observation) (observable observable))
	       (alloy:observe observation observable
			      (lambda (value data)
				(declare (ignore value data))
				(format t "Observed a change for ~a in ~a~%" observation observable)))))))



(presentations:define-update (presentations:default-look-and-feel alloy:button)
  (:label :text alloy:text
   :halign :start
   :size 10))

(presentations:define-update (presentations:default-look-and-feel alloy:label)
  (:label :text alloy:text
   :halign :start
   :size 10))


;; Convenience utilities
(defun randofoo () (format nil "foo-~a" (random 1000)))
(defun randobar () (format nil "bar-~a" (random 1000)))
(defun randofar () (format nil "far-~a" (random 1000)))
(defun randoboo () (format nil "boo-~a" (random 1000)))
(defun randounique () (format nil "unique-~a" (random 1000)))
