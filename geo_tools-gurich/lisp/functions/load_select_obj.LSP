(defun C:load_select_obj (/ ssnab x temp)
  (if *save-select-obj*
    (progn
      (sssetfirst)
      (setq ssnab (ssadd))
      (foreach x *save-select-obj*
	(setq temp(vlax-vla-object->ename x))
	(if temp
	  (setq ssnab (ssadd temp ssnab)))
	)
      (sssetfirst nil ssnab)
      (princ (strcat "\n����������� ����� "(itoa(sslength ssnab)) " ��������."))
      )
    (princ "\n��� ������������ ������ ��������.")
    )
  (princ)
  )