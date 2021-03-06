(defun C:edit_move_to_layer_unnecessary (/ last_layer flag layer_name temp n nabor n_y); ��������� ���������� ������� � �������� ����
  (geo_tools-help "edit_move_to_layer_unnecessary")
  (setq last_layer (PL:GetD "target_layer"))
  (if (or(eq last_layer nil)(not(tblobjname "LAYER" last_layer)))
	 (setq last_layer "0"))
  (setq flag T)
  (while flag
    (initget "������� �������")(setq layer_name (getstring T(strcat"\n����� ��� �������� ���� [�������/�������]<"last_layer">: ")))
    (cond
      ((eq layer_name "�������")
       (setq layer_name(dialog_seleckt_layer))
       (setq flag nil)
       )
      ((eq layer_name "�������")
       (setq layer_name(getvar "Clayer"))
       (setq flag nil)
       )
      ((eq layer_name "")
       (setq layer_name last_layer)
       (setq flag nil)
       )
      (T
       (if (not(tblobjname "layer" layer_name))
	 (progn
	   (initget "�� ���")(setq n_y (getkword "\n��� ���� � ����� ������, �������? [��/���]<���>: "))
	   (if (eq n_y "��")
	     (progn
	       (setq temp(getvar "Clayer"))
	       (command "_-LAYER" "�" layer_name "")
	       (setvar "Clayer" temp)
	       (setq flag nil)))
	   )
	 )
       )
      )
    )
  (PL:SetD "target_layer" layer_name)
  (setq nabor (ssget "_:S"))
  (while (not (eq nabor nil))
    (setq n 0)
    (repeat (sslength nabor)
      (vla-put-layer  (vlax-ename->vla-object(ssname nabor n)) layer_name)
      (setq n (1+ n))
      )
    (setq nabor (ssget "_:S"))
    )
  (princ)
  )