(in-package #:org.shirakumo.alloy.examples)

(define-example canvas (screen)
  "Different drawing operations on a canvas component."
  (let* ((window (windowing:make-window screen :min-size (alloy:px-size 700 480)))
         (canvas (make-instance 'simple:canvas :renderer screen :layout-parent window)))
    (setf (simple:pattern canvas) colors:white)
    (simple:fill-rectangle canvas 0 0 1000.0 1000.0)
    (setf (simple:pattern canvas) colors:red)
    (simple:draw-rectangle canvas 5 5 100 20)
    (setf (simple:pattern canvas) colors:orange)
    (simple:fill-rectangle canvas 10 15 100 30 '(0.0 20.0 10.0 2.0))
    (setf (simple:line-width canvas) 5.0)
    (setf (simple:join-style canvas) :round)
    (setf (simple:pattern canvas) colors:blue)
    (setf (simple:line-style canvas) :dotted)
    (simple:draw-rectangle canvas 15 20 60 50 '(0.0 10.0 5.0 2.0))
    (flet ((circles (a b)
             (setf (simple:pattern canvas) colors:black)
             (simple:fill-ellipse canvas 120 30 20 20)
             (setf (simple:pattern canvas) colors:green)
             (simple:fill-ellipse canvas 122 32 16 16 a b)
             (simple:draw-ellipse canvas 120 30 20 20 a b)
             (simple:translate canvas (alloy:point 25 0))))
      (setf (simple:line-width canvas) 2.0)
      (setf (simple:line-style canvas) :solid)
      (simple:push-matrix canvas)
      (circles 0.0 (* 2.0 PI))
      (circles 0.0 (* 0.5 PI))
      (circles 0.0 (* 1.0 PI))
      (circles 0.0 (* 1.5 PI))
      (setf (simple:line-style canvas) :dotted)
      (circles (* 0.5 PI) (* 1.0 PI))
      (circles (* 0.25 PI) (* 1.75 PI))
      (circles (* 1.75 PI) (* 0.25 PI))
      (setf (simple:line-style canvas) :dashed)
      (circles (* 1.5 PI) (* 0.25 PI))
      (circles (* 1.15 PI) (* 0.25 PI))
      (simple:pop-matrix canvas))
    (flet ((circles ()
             (setf (simple:pattern canvas) (colored:color 1.0 1.0 0.0 1.0))
             (simple:fill-ellipse canvas 80 30 20 20)
             (setf (simple:pattern canvas) (colored:color 1.0 0.0 1.0 1.0))
             (simple:fill-ellipse canvas 90 30 20 20)
             (setf (simple:pattern canvas) (colored:color 0.0 1.0 1.0 1.0))
             (simple:fill-ellipse canvas 85 40 20 20)))
      (setf (simple:pattern canvas) colors:black)
      (simple:fill-rectangle canvas 70 105 50 80)
      (simple:push-matrix canvas)
      (setf (simple:composite-mode canvas) :multiply)
      (simple:translate canvas (alloy:point 0 40))
      (circles)
      (setf (simple:composite-mode canvas) :screen)
      (simple:translate canvas (alloy:point 0 40))
      (circles)
      (setf (simple:composite-mode canvas) :difference)
      (simple:translate canvas (alloy:point 0 40))
      (circles)
      (simple:pop-matrix canvas)
      (setf (simple:composite-mode canvas) :source-over))
    (flet ((line ()
             (simple:translate canvas (alloy:point 0 15))
             (simple:start-line canvas 15 70)
             (simple:move-to canvas 30 90)
             (simple:move-to canvas 45 70)
             (simple:move-to canvas 60 90)
             (simple:complete-shape canvas)))
      (simple:push-matrix canvas)
      (setf (simple:pattern canvas) colors:black)
      (setf (simple:line-width canvas) 1.0)
      (line)
      (setf (simple:line-width canvas) 2.0)
      (line)
      (setf (simple:line-width canvas) 5.0)
      (setf (simple:line-style canvas) :dashed)
      (setf (simple:join-style canvas) :round)
      (setf (simple:cap-style canvas) :round)
      (line)
      (setf (simple:line-width canvas) 10.0)
      (setf (simple:line-style canvas) :dotted)
      (setf (simple:join-style canvas) :miter)
      (setf (simple:cap-style canvas) :spike)
      (line)
      (setf (simple:line-width canvas) 10.0)
      (setf (simple:line-style canvas) :solid)
      (setf (simple:join-style canvas) :bevel)
      (setf (simple:cap-style canvas) :square)
      (line))
    (simple:draw-curve canvas 15 100 60 100 60 130 15 130)
    (let ((gradient (simple:request-gradient screen :linear (alloy:point 100 0) (alloy:point 300 0)
                                             #((0.0 #.colors:red)
                                               (0.5 #.colors:green)
                                               (1.0 #.colors:blue)))))
      (setf (simple:pattern canvas) gradient)
      (setf (simple:line-style canvas) :dotted)
      (simple:fill-rectangle canvas 130 50 200 50 '(0 0 50 20))
      (simple:draw-rectangle canvas 125 45 210 60 '(0 0 50 20)))
    (let ((texture (simple:image-pattern screen (simple:request-image screen (file "logo.png"))
                                         :scaling (alloy:size 0.5 0.5))))
      (setf (simple:pattern canvas) texture)
      (setf (simple:line-style canvas) :dotted)
      (simple:fill-rectangle canvas 130 -15 200 50 '(0 0 50 20))
      (simple:draw-rectangle canvas 125 -20 210 60 '(0 0 50 20)))))
