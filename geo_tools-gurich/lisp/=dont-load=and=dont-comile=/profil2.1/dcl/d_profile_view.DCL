// �������� �������������� ���� �������
d_profile_view:dialog{label="�������� �������������� ���� �������";
:column{label="��� �������";
:popup_list{key="view-basement-name";list="";}
:row{
:button{label="�����";key="new-view-basement";}
:button{label="�������������";key="rename-view-basement";}
:button{label="�������";key="delete-view-basement";}
}
}
:row{label="���������� �������";
:column{
:list_box{label="������";key="profile-strings";list="";height=15;width=30;}
}
:column{
spacer_1;
:button{label="<< ��������";key="add";}
:button{label="�����";key="up";}
:button{label="����";key="down";}
:button{label="< �������������";key="edit";}
:button{label="�������";key="del";}
spacer_1;
:button{label="������������� >";key="edit_standart_str";fixed_height=true;}

}
:column{
:list_box{label="����������� ������";key="list-standart-str";list="";height=15;width=30;}
}
}
:column{label="��������� �������� ���� �������";
:row{
:toggle{label="�������� �������";key="draw-ruler";value="1";}
:edit_box{label="������ ������ ���������� � ��";key="h-title-text";value="";edit_width=5;}
}
:edit_box{label="������ ����� ���������� ������� � ��";key="width-basement";value="";edit_width=5;}
:edit_box{label="���������� �� ������� �� ������ ������� � ��";key="buffer-dist";value="";edit_width=5;}
:popup_list{label="��������� ����� �������";key="text-style";list="";}
:column{label="������ �������� ������������� �������";
:edit_box{label="��������";key="basement-str-title";value="";edit_width=40;}
:edit_box{label="������� ����� ������������ ���������";key="basement-str-vertical-scale";value="";edit_width=40;}
:edit_box{label="������� ����� �������������� ���������";key="basement-str-horizontal-scale";value="";edit_width=40;}
:edit_box{label="������� ����� �������� ����������";key="basement-str-reference-level";value="";edit_width=40;}
:edit_box{label="������ ������ ��������� � ��";key="h-intersection-text";value="";edit_width=5;}
:popup_list{label="����� ����� ������� � ��������";key="accuracy-mleader";list="0\n0.0\n0.00\n0.000";}
}
}
ok_cancel;
}