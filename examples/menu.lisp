(in-package #:org.shirakumo.alloy.examples)

(define-example menu-bar (screen)
  (let* ((window (windowing:make-window screen))
         (layout (make-instance 'alloy:border-layout :layout-parent window))
         (focus (make-instance 'alloy:focus-list :focus-parent window))
         (menu (alloy:with-menu
                 ("File"
                  ("Save" (print "Saved."))
                  ("Save As..." (print "Save As..."))
                  ("Open" (print "Open"))
                  ("Open Recent"
                   ("1" (print "Open 1"))
                   ("2" (print "Open 2")))
                  ("Quit" (windowing:close window)))
                 ("Help"
                  ("About" (print "About"))))))
    (alloy:enter menu layout :place :north :size (alloy:un 20))
    (alloy:enter menu focus)))
