// �������� ����� �� ������
d_add_piket: dialog{label="��������� ���������� ���. �������";initial_focus="start";

:popup_list{label="�������� �������";key="accuracy-picket-angle";list="0\n0.0\n0.00\n0.000";value="0";}
  :toggle{label="�������� ����������";key="draw-coor";value="0";}
  :column{label="����������";
  :popup_list{label="����� ����� �������";key="numbers-coor";
  list="��� ���������\n123\n1234\n12345\n123456\n1234567\n12345678";value="0";}
  :popup_list{label="����� ����� �������";key="accuracy-coor";
  list="0\n0.0\n0.00\n0.000";value="2";}
  }
  :row{
  :edit_box{label="��������� ����� ������";key="picket-first";value="0";edit_width=6;}
  :text{label="�";}
  }
  ok_cancel;
  }