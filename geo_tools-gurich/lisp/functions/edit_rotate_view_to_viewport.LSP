(defun C:edit_rotate_view_to_viewport ( / t1 t2 a)
(geo_tools-help "edit_rotate_view_to_viewport")
  (terpri)
  (initget)(setq t1(getpoint "Укажи первую точку:")); указываю точку
  (terpri)
  (initget)(setq t2(getpoint t1 "Укажи вторую точку:")); указываю вторую точку
  (setq a (angtos (+ (/ pi 2)(angle t1 t2)) 2)); получаем угол поворота
  (command "_dview" "" "_tw" a ""); поворачиваем
  )
