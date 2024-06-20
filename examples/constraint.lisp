(in-package #:org.shirakumo.alloy.examples)

(define-example constraint-layout (screen)
  (let* ((window (windowing:make-window screen))
         (focus (make-instance 'alloy:focus-list :focus-parent window))
         (layout (make-instance 'org.shirakumo.alloy.layouts.constraint:layout :layout-parent window))
         (button (alloy:represent "Greet" 'alloy:button))
         (button2 (alloy:represent "Hi" 'alloy:button))
         ;;
         (fixed (alloy:represent "fixed" 'alloy:button :style `((:background . (:pattern ,colors:deep-pink)))
                                                       :sizing-strategy (make-instance 'alloy:fixed-size
                                                                                       :fixed-size (alloy:px-size 80 80))
                                                       :focus-parent focus))
         (height 200)
         (custom-height (alloy:represent height 'alloy:ranged-wheel
                                         :range '(1 . 300)
                                         :placeholder "height"
                                         :style `((:background . (:pattern ,colors:deep-pink)))
                                         :sizing-strategy (lambda (component suggested-size)
                                                            (declare (ignore component suggested-size))
                                                            (alloy:px-size 80 height))
                                         :focus-parent focus))
         (square (alloy:represent "square" 'alloy:button
                                  :style `((:background . (:pattern ,colors:deep-pink)))
                                  :sizing-strategy (lambda (component suggested-size)
                                                     (declare (ignore component))
                                                     (let ((min (/ (+ (alloy:pxw suggested-size)
                                                                      (alloy:pxh suggested-size))
                                                                   2)))
                                                       (alloy:px-size min min)))
                                  :focus-parent focus))
         (center-p T)
         (center-or-full-width (alloy:represent "center or full width" 'alloy:button :focus-parent focus)))
    (:inspect layout)
    ;; one case you might want to add is a centered element that has a maximum size
    (alloy:enter button layout :constraints '(:contained (:left 0) (:top 0) (:width 100) (:height 100)))
    (alloy:enter button2 layout :constraints `((:chain :right ,button 10) (:width 100)))
    (alloy:enter button focus)

    (alloy:enter fixed layout :constraints `((= :x 0)
                                             (= (+ :y :h) (- (:y ,button) 10))))
    (alloy:enter custom-height layout :constraints `((= :x (+ (:x ,fixed) (:w ,fixed) 10))
                                                     (= (+ :y :h) (+ (:y ,fixed) (:h ,fixed)))))
    (alloy:on alloy:value (new-value custom-height)
      (declare (ignore new-value))
      (alloy:notice-size custom-height T))
    (alloy:enter square layout :constraints `((:chain :right ,custom-height 10)))

    ;; centering example
    (flet ((compute-constraints (center-p)
             `(,(if center-p
                    `(:required (:center :x))
                    `(:required (= :w :rw)))
               (:bottom 10)
               (:height 50))))
      (alloy:enter center-or-full-width layout
                   :constraints (compute-constraints T ; center-p
                                                     ))
      (alloy:on alloy:activate (center-or-full-width)
        (setf center-p (not center-p))
        (alloy:update center-or-full-width layout
                      :clear T
                      :constraints (compute-constraints T ; center-p
                                                        ))))))
