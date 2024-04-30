(in-package #:org.shirakumo.alloy.examples)

(define-example grid-bag-layout (screen)
  (let* ((window (windowing:make-window screen :preferred-size (alloy:px-size 800 600)))
         (focus (make-instance 'alloy:focus-list :focus-parent window))
         ;; TODO keep this?
         (layout0 (make-instance 'org.shirakumo.alloy:grid-layout :layout-parent window
                                                                  :row-sizes '(T) :col-sizes '(T)))
         (layout (make-instance 'org.shirakumo.alloy:grid-bag-layout
                                :row-sizes '(30 50 T 30 30)
                                :col-sizes '(50 50 50 T)))
         ;; Growth direction
         (label (alloy:represent "Grow" 'alloy:label) )
         (growth (alloy:represent (alloy::growth-policy layout) 'alloy:combo-set
                                  :value-set '(nil :horizontal :vertical :both)
                                  :focus-parent focus))
         ;; Adding/removing rows/cols
         (row-number -1)
         (select-row (alloy:represent row-number 'alloy:ranged-wheel :range '(-1 . 1000) :focus-parent focus))
         (add-row (alloy:represent "Add Row" 'alloy:button :focus-parent focus))
         (remove-row (alloy:represent "Remove Row" 'alloy:button :focus-parent focus))
         (col-number -1)
         (select-col (alloy:represent col-number 'alloy:ranged-wheel :range '(-1 . 1000) :focus-parent focus))
         (add-col (alloy:represent "Add Col" 'alloy:button :focus-parent focus))
         (remove-col (alloy:represent "Remove Col" 'alloy:button :focus-parent focus))
         (add-component (alloy:represent "Add Component" 'alloy:button :focus-parent focus))
         ;; Elements that demonstrate specific features
         (span-rows (alloy:represent "Span rows?" 'alloy:button :focus-parent focus))
         (span-rows-p T)
         (kill-me (alloy:represent "Kill me now" 'alloy:button :focus-parent focus))
         (wrong (alloy:represent "Wrong" 'alloy:button :focus-parent focus)))
    (alloy:enter layout layout0)
    ;; In the "cell coordinate system" which the grid bag layout uses and to
    ;; which the following positions and sizes refer, the direction of the
    ;; positive y axis is downwards (in contrast to other Alloy coordinate
    ;; systems).
    (alloy:enter label layout :x 0 :y 0)
    (alloy:enter growth layout :x 1 :y 0 :w 3)
    (alloy:enter kill-me layout :x 0 :y 1 :w 4)
    (alloy:enter add-component layout :x 0 :y 2 :w 3)
    (alloy:enter select-row layout :x 0 :y 3)
    (alloy:enter add-row layout :x 1 :y 3)
    (alloy:enter remove-row layout :x 2 :y 3)
    (alloy:enter select-col layout :x 0 :y 4)
    (alloy:enter add-col layout :x 1 :y 4)
    (alloy:enter remove-col layout :x 2 :y 4)
    (alloy:enter span-rows layout :x 3 :y 2 :h (if span-rows-p 3 1))
    ;; This must signal an error because WRONG would occupy a cell that is
    ;; already occupied by SPAN-ROWS.
    (handler-case
        (alloy:enter wrong layout :x 2 :y 2)
      (error (condition)
        (princ condition *trace-output*)))
    ;;
    (alloy:on alloy:activate (kill-me)
      (alloy:leave kill-me layout)
      (alloy:leave kill-me focus))
    ;; Test adding rows
    (alloy:on alloy:activate (add-row)
      (apply #'alloy:add-row layout (alloy:un 5)
             (when (/= row-number -1) (list row-number))))
    (alloy:on alloy:activate (remove-row)
      (let ((row-number (if (= row-number -1)
                            (1- (length (alloy:row-sizes layout)))
                            row-number)))
        (alloy:remove-row layout row-number)))
    (alloy:on alloy:activate (add-col)
      (apply #'alloy:add-column layout (alloy:un 5)
             (when (/= col-number -1) (list col-number))))
    (alloy:on alloy:activate (remove-col)
      (let ((col-number (if (= col-number -1)
                            (1- (length (alloy:col-sizes layout)))
                            col-number)))
        (alloy:remove-column layout col-number)))
    ;; Test updating component layout info
    (alloy:on alloy:activate (span-rows)
      (setf span-rows-p (not span-rows-p))
      (multiple-value-bind (y h)
          (cond ((not span-rows-p)
                 (values 2 1))
                ((ignore-errors
                  (alloy:element-index
                   kill-me layout))
                 (values 2 3))
                (t
                 (values 1 4)))
        (alloy:update span-rows layout :x 3 :y y :h h)))
    ;; Test for adding components
    (let ((next-id 1))
      (alloy:on alloy:activate (add-component)
        (let* ((label (format nil "# ~D" next-id))
               (button (alloy:represent label 'alloy:button :focus-parent focus)))
          (alloy:enter button layout :h 1 ; (+ 1 (random 3))
                       )
          (incf next-id))))))
