// ������� ������������
dialog_draw_elevation_contour_line: dialog{label="��������� ������� ������������";initial_focus="number-first";
:popup_list{label="����������� �������";key="input-mode";list="������ ����� 0\n������\n�������";}
:toggle{label="����� �������� ������� �����������";key="smart-accuracy-elevation";value="1";}
:popup_list{label="�������� �������";key="accuracy-elevation";list="0\n0.0\n0.00\n0.000";}
ok_cancel;
}