(in-package #:org.shirakumo.alloy)

(define-condition alloy-condition (condition) ())

(defmacro define-alloy-condition (name superclasses format &body slotsargs)
  (let ((slots (loop for slot in slotsargs
                     unless (listp slot)
                     collect (list slot :initarg (intern (string slot) "KEYWORD") :reader slot)))
        (args (loop for arg in slotsargs
                    collect (if (listp arg) arg `(,arg c)))))
    `(define-condition ,name (,@superclasses alloy-condition)
       ,slots
       ,@(when format
           `((:report (lambda (c s) (format s ,format ,@args))))))))

(define-alloy-condition argument-missing (error)
    "The initarg ~s is required but was not passed."
  initarg)

(defun arg! (initarg)
  (error 'argument-missing :initarg initarg))

(define-alloy-condition index-out-of-range (error)
    "The index ~d is outside of [~{~d~^,~}[."
  index range)

(define-alloy-condition hierarchy-error (error)
    NIL container bad-element)

(define-alloy-condition element-has-different-parent (hierarchy-error)
    "Cannot perform operation with~%  ~s~%on~%  ~s~%as it is a child on~%  ~s"
  bad-element container parent)

(define-alloy-condition element-already-contained (hierarchy-error)
    "Cannot enter~%  ~s~%on~%  ~s~%as it is already contained within."
  bad-element container)

(define-alloy-condition element-not-contained (hierarchy-error)
    "The element~%  ~s~%is not a child of~%  ~s"
  bad-element container)

(define-alloy-condition element-has-different-root (hierarchy-error)
    "The element~%  ~s~%comes from the tree~%  ~s~%which is not~%  ~s"
  bad-element (focus-tree (bad-element c)) container)

(define-alloy-condition root-already-established (error)
    "Cannot set~%  ~a~%as the root of~%  ~a~%as it already has a root in~%  ~a"
  bad-element tree (root (tree c)))

(define-alloy-condition layout-condition ()
    NIL layout)

(define-alloy-condition place-already-occupied (layout-condition error)
    "Cannot enter~%  ~a~%at ~a into~%  ~a~%as it is already occupied by~%  ~a"
  bad-element place layout existing)

(define-alloy-condition place-does-not-exist (layout-condition error)
    "Cannot enter~%  ~a~%at ~a into~%  ~a~%as the current dimensions~%  ~a~%~
     of the layout do not include that place and the layout is not allowed ~
     to grow according to the policy~%  ~a"
  bad-element place layout dimensions growth-policy)

(define-alloy-condition layout-cannot-grow (layout-condition error)
    "Layout~%  ~a~% is not allowed to grow ~(~a~)ly according to policy~%  ~a"
  layout direction growth-policy)

(define-alloy-condition element-has-different-ui (error)
    "The element~%  ~s~%cannot be used with~%  ~s~%as it is set-up with~%  ~s"
  bad-element ui (ui (bad-element c)))

(define-alloy-condition allocation-failed (error)
    "The allocation of the renderer~%  ~s~%failed."
  renderer)
