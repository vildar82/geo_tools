(defun distance-to-horizon (oblique-distance vertical-angle / )
  ;���������� ���������� ���������� � ��������� (���� � ���������� ��������)
  (* oblique-distance(cos(convert-grad-rad vertical-angle)))
  )