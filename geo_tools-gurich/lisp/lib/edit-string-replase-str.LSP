(defun edit-string-replase-str (str-new str-old string /)
  ; ����������� ������ ������� ��� ������������� ������ str-old �� str-new
   (while (vl-string-search str-old string)
    (setq string (vl-string-subst str-new str-old string))
    )
  string
  )