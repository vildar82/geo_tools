(defun calculation-h-trig-nivelir (oblique-distance vertical-angle h-instrument h-target / )
  ;возвращает превышение при тригонометрическом нивилировании требуется угол в десятичных градусах
  (-(+(* (distance-to-horizon oblique-distance vertical-angle) (tg (convert-grad-rad vertical-angle)))h-instrument)h-target)
  )