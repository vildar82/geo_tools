(defun analisis-addres-from-type (string /)
  (cond
    ((or(vl-string-search "������" string )(vl-string-search "������" string )(vl-string-search "������" string ))
     "������"
     )
    ((or(vl-string-search "����������" string )(vl-string-search "����������" string )(vl-string-search "����������" string ))
     "����������"
     )
    ((or(vl-string-search "��������" string )(vl-string-search "��������" string )(vl-string-search "��������" string ))
     "��������"
     )
    ((or(vl-string-search "����" string )(vl-string-search "����" string )(vl-string-search "����" string ))
     "����"
     )
    ((vl-string-search "��" string )
     "��"
     )
    ((or(vl-string-search "�����" string )(vl-string-search "�����" string )(vl-string-search "�����" string )
	(vl-string-search "�P��������" string )(vl-string-search "�p��������" string )(vl-string-search "�p��������" string ))
     "�����.�������"
     )
    
    ((or(vl-string-search "�����" string )(vl-string-search "�����" string )(vl-string-search "�����" string ))
     "�����"
     )
    ((or(vl-string-search "�����" string )(vl-string-search "�����" string )(vl-string-search "�����" string ))
     "�����"
     )
    ((or(vl-string-search "������" string )(vl-string-search "������" string )(vl-string-search "������" string ))
     "������"
     )
    ((or(vl-string-search "������" string )(vl-string-search "������" string )(vl-string-search "������" string ))
     "������"
     )
    )
  )