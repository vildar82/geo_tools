// �������� �������������� ���� �������
d_profile_view:dialog{label="�������� �������������� ���� �������";
:column{label="��� �������";
:popup_list{key="profile-name";list="";}
:row{
:button{label="�����";key="new";}
:button{label="�������������";key="rename";}
:button{label="�������";key="delete";}
}
}
:row{label="���������� �������";
:column{
:list_box{label="������";key="list-podval-str";list="";height=15;width=30;}
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
:toggle{label="�������� �������";key="flag_ruler";value="1";}
:edit_box{label="������ ������ ���������� � ��";key="h-txt";value="";edit_width=5;}
}
:edit_box{label="������ ����� ���������� ������� � ��";key="d-basement";value="";edit_width=5;}
:edit_box{label="���������� �� ������� �� ������ ������� � ��";key="buffer-dist";value="";edit_width=5;}
:popup_list{label="��������� ����� �������";key="text-style";list="";}
:column{label="������ �������� ������������� �������";
:edit_box{label="��������";key="str4";value="";edit_width=40;}
:edit_box{label="������� ����� ������������ ���������";key="str3";value="";edit_width=40;}
:edit_box{label="������� ����� �������������� ���������";key="str2";value="";edit_width=40;}
:edit_box{label="������� ����� �������� ����������";key="str1";value="";edit_width=40;}
:edit_box{label="������ ������ ��������� � ��";key="h-txt-intersect";value="";edit_width=5;}
:popup_list{label="����� ����� ������� � ��������";key="accuracy-mleader";list="0\n0.0\n0.00\n0.000";}
}
}
ok_cancel;
}