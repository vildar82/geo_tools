(defun draw_text_vla ;
       ; �������� ��������� Text ���������� ActiveX
       ;������ (draw_text_vla active_document "�����" '(0 0 0) 2.5 T T nil T T)
       (spece TextString Point Height Alignment Rotation Linetype Layer Lineweight / text)
  (load_global_variable)
  (setq text (vla-AddText spece TextString (vlax-3D-point Point) Height))
  ; ��� Alignment - T �� ��� ����� �� ����������, ���� nil �� =4 (��������) � ����������� TextAlignmentPoint
  ; ����� �������� ������������ ��� Alignment
  (if Alignment
    (if (not (eq (type Alignment) 'SYM))
      (progn
	(vla-put-Alignment text Alignment)
	(vla-put-TextAlignmentPoint text (vlax-3D-point Point))
	)
      )
    (progn
      (vla-put-Alignment text 4)
      (vla-put-TextAlignmentPoint text (vlax-3D-point Point))
      )
    )
  ; ��� Rotation - nil ��� ����� ���������� (/ pi 2) ��� T �� ��������� - 0
  ; ��� ��������� ��������� ��� ����� ������� �� ���� ����������
  (if Rotation
    (if (not (eq (type Rotation) 'SYM))
      (vla-put-Rotation text Rotation)
      )
    (vla-put-Rotation text (/ pi 2))
    )
  ; ��� Linetype - nil ��� ����� ���������� "Continuous" ��� T �������
  ; ��� ��������� ��������� ��� ����� ������� �� ���� ����������
  (if Linetype
    (if (not (eq (type Linetype) 'SYM))
      (vla-put-Linetype text Linetype)
      )
    (vla-put-Linetype text "Continuous")
    )
  ; ��� Layer - nil ���� ���������� "��������" ��� T �������
  ; ��� ��������� ��������� ��� ����� ������� �� ���� ����������
  (if Layer
    (if (not (eq (type Layer) 'SYM))
      (vla-put-Layer text Layer)
      )
    (vla-put-Layer text "��������")
    )
  ; ��� Lineweight - nil ������� ���������� 0,3 ��� T �������
  ; ��� ��������� ��������� ��� ����� ������� �� ���� ����������
  (if Lineweight
    (if (not (eq (type Lineweight) 'SYM))
      (vla-put-Lineweight text Lineweight)
      )
    (vla-put-Lineweight text 30)
    )
  text
  )
