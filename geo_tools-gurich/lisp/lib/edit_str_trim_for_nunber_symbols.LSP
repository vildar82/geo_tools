(defun edit_str_trim_for_nunber_symbols (str nunber nil-left/-T-right /)
  ; ��������� ��������� ������ �� ���������� ����� ��������, ����� �������� ����� � ������
  (while (< nunber (strlen str))
    (if nil-left/-T-right
      (setq str (substr str 1 (1-(strlen str))))
      (setq str (substr str 2 (strlen str)))
      )
    )
  str
  )