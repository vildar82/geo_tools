(defun convert-string-to-bool ( item / )
  ; ������������ "1" � T (������), ��� ��������� � nil (����)
  (if (= item "1")
    T
    nil
    )
  )