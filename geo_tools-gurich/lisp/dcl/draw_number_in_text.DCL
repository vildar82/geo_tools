// ���������
draw_number_in_text: dialog{label="��������� ���������";initial_focus="number-first";
:popup_list{key="mode";list="������� �������\n�������� � ���. �������\n�������� ����������\n�������� ������� ���������";}
:popup_list{key="create-type";list="�����\n������\n��������";}
:popup_list{key="create-mode";list="";}

:column{label="����� ������";
:edit_box{label="�������";key="perfix";value="";edit_width=7;}
:edit_box{label="������ �";key="number-first";value="1";edit_width=3;}
:edit_box{label="�������";key="suffix";value="";edit_width=7;}
}
:edit_box{label="������ ������ � ��";key="h-text";value="";edit_width=3;}
ok_cancel;
}