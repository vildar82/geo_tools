(defun get-string-or-text(/ flag obj string)
  (load_global_variable)
  (setq flag T)
  (while flag
    (setq obj (car(entsel "\n����� ������ ���� �����<������>: ")))
    (cond
      ((eq obj nil)
       (setq string (getstring t "\n����� ����� ���������: "))
       (setq flag nil)
       )
      ((eq (cdr(assoc 0 (entget obj))) "TEXT")
       (setq string(cdr(assoc 1(entget obj))))
       (setq flag nil)
       )
      (T
       (alert "������! ������ �� ������ �� ����������� ����. ����������� ������������ ����� � ����� COGO. �������� �����.")
       )
      )
    )
  string
  )