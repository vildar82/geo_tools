(defun get-civil-or-not ( / var-acad-ver)
  ; ���������� ������� AutoCad Civil ��� ���, ����������� "�����������" �� ����������� � ���� � acad.exe ����� "Civil"
  (if
    (vl-string-search "Civil" (vla-get-Path (vlax-get-acad-object)))
    T
    nil
    )
  )