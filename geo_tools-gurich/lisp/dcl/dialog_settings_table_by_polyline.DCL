// �������� ������ �������
dialog_settings_table_by_polyline:dialog{label="��������� ������� ���������";
:toggle{label="������ �������� � �������� ��������";key="mleader-enable";value="1";}
:row{
:toggle{label="������� ��� ������� ���������";key="add-perfix-to-mleader";value="1";}
:edit_box{label="�������";key="perfix";value="�.";edit_width=3;}
}
:toggle{label="���������� ��������� ��� ���������� � ��� �������";key="auto-numering";value="1";}
:row{
:edit_box{label="�������� ��������� �";key="first-number";value="1";edit_width=3;}
:popup_list{label="�������� ���������";key="accuracy-coordinate";list="0\n0.0\n0.00\n0.000";edit_width=5;}
}
:toggle{label="�������� ������� �����";key="angle-column";value="0";}
ok_cancel;
}