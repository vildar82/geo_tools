(defun C:profile_calculation_basement ( / block-profile-obj curent-profile result-dialog)
  (load_global_variable)
  (geo_tools-help "profile_calculation_basement")
  (setq curent-profile(vlax-ldata-get "geo_tools_dictionary" "curent-profile"))
  (if curent-profile
    (if (vlax-erased-p (cdr curent-profile))
      (alert "������! ������� ������� ��� ������.\n������� ������ ������� ������� ����� � ������� ������� \"������� ������� �������\"")
      (progn
	(princ (strcat"\n������� �������: "(car curent-profile)))
	(setq block-profile-obj (cdr curent-profile))
	(setq result-dialog (profile-dialog-calculation-basement block-profile-obj))
	)
      )
    (alert "������! ������� ������� �� ���������.\n������� ������� ������� ����� � ������� ������� \"������� ������� �������\"")
    )
  (princ)
  )
  