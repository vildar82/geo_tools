(defun profile-check-curent-profile (curent-profile / result)
  (if curent-profile
    (if (vlax-erased-p (cdr curent-profile))
      (alert "������! ������� ������� ��� ������.\n������� ������ ������� ������� ����� � ������� ������� \"������� ������� �������\"")
      (setq result T)
      )
    (alert "������! ������� ������� �� ���������.\n������� ������� ������� ����� � ������� ������� \"������� ������� �������\"")
    )
  result
  )