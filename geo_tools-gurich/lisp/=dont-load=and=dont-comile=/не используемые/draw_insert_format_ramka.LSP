(defun C:draw_insert_format_ramka (/ block_name)
(geo_tools-help "draw_insert_format_ramka")
  (initget 1 )(setq block_name (getstring T "\n����� ��� �����: "))
  (draw_blok_ramka block_name "���" nil T nil)
  (princ)
  )
