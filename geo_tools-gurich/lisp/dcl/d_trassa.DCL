// ���������� ������
d_trassa: dialog{label="��������� ���������� ������";//initial_focus="n";

:column{label="�������";
  :toggle{label="������� �������";key="draw-picket";value="0";}
  :popup_list{label="�������� �������";key="accuracy-picket";list="0\n0.0\n0.00\n0.000";value="0";}
  :toggle{label="������ � ��������� �����";key="draw-start-end-picket";value="0";}
  }
  :column{label="���� �������� (��)";
  :toggle{label="��������� ������� ��";key="draw-mleader";value="0";}
  :popup_list{label="�������� �����";key="accuracy-angle";list="0d\n0d00'\n0d00'00\"";value="0";}
  :edit_box{label="������� ����";key="perfix-angle";value="";edit_width=30;}
  :popup_list{label="�������� �������";key="accuracy-picket-angle";list="0\n0.0\n0.00\n0.000";value="0";}
  :toggle{label="������ � ��������� �����";key="draw-start-end-picket-angle";value="0";}
  :edit_box{label="����������� ������������ ����";key="min-angle";value="00d00'00\"";edit_width=7;}
  }
  :column{label="����������";
  :toggle{label="�������� ����������";key="draw-coor";value="0";}
  :popup_list{label="����� ����� �������";key="numbers-coor";list="��� ���������\n123\n1234\n12345\n123456\n1234567\n12345678";value="0";}
  :popup_list{label="�������� ���������";key="accuracy-coor";list="0\n0.0\n0.00\n0.000";value="0";}
  }
  :row{
  :edit_box{label="��������� ����� ������";key="picket-first";value="0";edit_width=8;}
  :text{label="�";}
  }
    :row{
  :edit_box{label="������� ��������� � �����";key="width-poly";value="0.3";edit_width=5;}
  :text{label="��";}
  }
  
  :toggle{label="������� ��������������� 2D���������";key="del-bak-2poly";value="0";}
  ok_cancel;
  }