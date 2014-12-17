(defun distance-to-horizon (oblique-distance vertical-angle / )
  ;приведение наклонного расстояния к горизонту (угол в десятичных градусах)
  (* oblique-distance(cos(convert-grad-rad vertical-angle)))
  )