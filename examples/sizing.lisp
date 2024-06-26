(in-package #:org.shirakumo.alloy.examples)

(define-example sizing (screen)
  (let* ((window (windowing:make-window screen :preferred-size (alloy:px-size 1024 768)))
         (focus (make-instance 'alloy:focus-list :focus-parent window))
         (layout (make-instance 'org.shirakumo.alloy:vertical-linear-layout))
         ;; Selected component, if any.
         (selected NIL)
         ;; Container for style-changing components.
         (style (make-instance 'org.shirakumo.alloy:grid-bag-layout :row-sizes #(30 30 30)
                                                                    :col-sizes #(T T T T)))
         ;; Explanatory label.
         (explanation (alloy:represent #.(format NIL "Click a blue button or switch (or this label), ~
                                                      then use the widgets to change ~
                                                      its properties.")
                                       'alloy:label :wrap t :focus-parent focus))
         ;; Four margins for the selected component.
         (margin-l 0)
         (select-margin-l (alloy:represent margin-l 'alloy:ranged-wheel :range '(0 . 100)
                                                                        :placeholder "margin left"
                                                                        :focus-parent focus))
         (margin-u 0)
         (select-margin-u (alloy:represent margin-u 'alloy:ranged-wheel :range '(0 . 100)
                                                                        :placeholder "margin top"
                                                                        :focus-parent focus))
         (margin-r 0)
         (select-margin-r (alloy:represent margin-r 'alloy:ranged-wheel :range '(0 . 100)
                                                                        :placeholder "margin right"
                                                                        :focus-parent focus))
         (margin-b 0)
         (select-margin-b (alloy:represent margin-b 'alloy:ranged-wheel :range '(0 . 100)
                                                                        :placeholder "margin bottom"
                                                                        :focus-parent focus))
         ;; Horizontal and vertical alignment for the selected component.
         (h-align :middle)
         (select-h-align (alloy:represent h-align 'alloy:combo-set
                                          :value-set '(:start :left :middle :center :end :right)
                                          :focus-parent focus))
         (v-align :middle)
         (select-v-align (alloy:represent v-align 'alloy:combo-set
                                          :value-set '(:top :middle :bottom)
                                          :focus-parent focus))
         (text-style NIL)
         (select-text-style (alloy:represent text-style 'alloy:combo-set
                                             :value-set '(NIL :italic :bold :strike :underline :fixed)
                                             :focus-parent focus))
         (text-size 16)
         (select-text-size (alloy:represent text-size 'alloy:ranged-wheel :range '(1 . 60)
                                                                          :placeholder "font size"
                                                                          :focus-parent focus))
         ;; Components whose style can be changed.
         (vertical (make-instance 'org.shirakumo.alloy:vertical-linear-layout))
         (button-v1 (alloy:represent "ButtonV1" 'alloy:button :focus-parent focus))
         (button-v2 (alloy:represent "ButtonV2" 'alloy:button :focus-parent focus))
         (button-v3 (alloy:represent #.(format NIL "Button~%V3") 'alloy:button :focus-parent focus))

         (horizontal (make-instance 'org.shirakumo.alloy:horizontal-linear-layout))
         (button-h1 (alloy:represent "ButtonH1" 'alloy:button :focus-parent focus))
         (button-h2 (alloy:represent "ButtonH2" 'alloy:button :focus-parent focus))
         (button-h3 (alloy:represent #.(format NIL "Button~%H3") 'alloy:button :focus-parent focus))
         (value1 NIL)
         (switch1 (alloy:represent value1 'alloy:switch :focus-parent focus))
         (value2 NIL)
         (switch2 (alloy:represent value2 'alloy:checkbox :focus-parent focus))
         (value3 NIL)
         (switch3 (alloy:represent value3 'alloy:labelled-switch :focus-parent focus
                                                                 :text "This is a long label tbh"))
         ;; User-supplied sizing strategies
         (horizontal2 (make-instance 'org.shirakumo.alloy:horizontal-linear-layout))
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
                                                     (let ((max (max (alloy:pxw suggested-size)
                                                                     (alloy:pxh suggested-size))))
                                                       (alloy:px-size max max)))
                                  :focus-parent focus))
         (forced-text-size (alloy:represent "much too long" 'alloy:label
                                            :style `((:label . (:bounds ,(alloy:extent 20 20 20 20)))
                                                     (:background . (:pattern ,colors:deep-pink)))))
         (outside-text (alloy:represent "text outside" 'alloy:label
                                        :style `((:label . (:bounds ,(alloy:point -20 -20)))
                                                 (:background . (:pattern ,colors:deep-pink))))))
    ;; Overall layout hierarchy
    (alloy:enter layout window)
    (alloy:enter explanation layout)
    (alloy:enter style layout)
    (alloy:enter vertical layout)
    (alloy:enter horizontal layout)
    (alloy:enter horizontal2 layout)
    ;; Style logic and changing components
    (labels ((set-style ()
               (when selected
                 (let ((text (alloy:value selected)))
                   (setf (org.shirakumo.alloy.renderers.simple.presentations:update-overrides selected)
                         `((:label . (:bounds ,(alloy:px-margins
                                                margin-l margin-u margin-r margin-b)
                                      :halign ,h-align
                                      :valign ,v-align
                                      :markup ,(if text-style
                                                   `((3 ,(length text) ,text-style))
                                                   NIL)
                                      :size ,(alloy:un text-size)))))
                   (alloy:notice-size selected (alloy:layout-parent selected)))))
             (add-style-aspect (component x y &rest args)
               (apply #'alloy:enter component style :x x :y y args)
               (alloy:on alloy:value (new-value component)
                 (declare (ignore new-value))
                 (set-style))))
      (add-style-aspect select-text-style 1 1)
      (add-style-aspect select-text-size 2 1)
      (add-style-aspect select-margin-l 0 1)
      (add-style-aspect select-margin-u 1 0 :w 2)
      (add-style-aspect select-margin-r 3 1)
      (add-style-aspect select-margin-b 1 2 :w 2)
      (add-style-aspect select-h-align 0 2)
      (add-style-aspect select-v-align 3 2))
    ;; Components which can be selected and for which the style can be changed.
    (labels ((make-selectable (component)
               (alloy:on alloy:activate (component)
                 (setf selected component)))
             (add-button (button parent)
               (alloy:enter button parent)
               (make-selectable button)))
      (make-selectable explanation)
      (loop for button in (list button-v1 button-v2 button-v3)
            do (add-button button vertical))
      (loop for button in (list button-h1 button-h2 button-h3
                                switch1 switch2 switch3)
            do (add-button button horizontal)))
    ;;
    (alloy:enter fixed horizontal2)
    (alloy:enter custom-height horizontal2)
    (alloy:on alloy:value (new-value custom-height)
      (declare (ignore new-value))
      (alloy:notice-size custom-height T))
    (alloy:enter square horizontal2)
    (alloy:enter forced-text-size horizontal2)
    (alloy:enter outside-text horizontal2)))
