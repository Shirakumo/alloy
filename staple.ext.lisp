(asdf:load-system :staple-markless)
(let ((subsystems '(alloy-windowing
                    alloy-simple
                    alloy-simple-presentations
                    ;alloy-svg
                    alloy-opengl
                    alloy-opengl-fond
                    alloy-glfw
                    alloy-constraint)))
  (mapcar #'asdf:load-system subsystems)

  (defmethod staple:subsystems ((_ (eql (asdf:find-system :alloy))))
    (mapcar #'asdf:find-system subsystems)))
