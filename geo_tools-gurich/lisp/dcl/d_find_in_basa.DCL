// ����� � ���� �������� �������
d_find_in_basa:dialog{label="�����";
:edit_box{label="���";key="name";value="";edit_width=20;}
:row{
:popup_list{label="��� ������";key="type";list="�� �����\n�� ������\n��.�����\n��������\n����������";}
:edit_box{label="���";key="year";value="";edit_width=6;}
}
:row{label="����������";
:column{
:edit_box{label="�������� ����������";key="N";value="";edit_width=20;}
:edit_box{label="��������� ����������";key="E";value="";edit_width=20;}
:edit_box{label="�������";key="Z";value="";edit_width=20;}
:spacer{height=0.1;}
}
:column{
:edit_box{label="�������� ���������";key="accuracy";value="";edit_width=2;}
:text{label="(� ������� ��������)";}
:spacer{height=1.5;}
}
}
:edit_box{label="�����, ��������������";key="address";value="";edit_width=40;}
:edit_box{label="����������";key="comment";value="";}
:button{label="������";key="save";is_default=true;is_cancel=false;fixed_width=true;alignment=right;}
:ok_button{label="������";is_cancel=true;is_default=false;alignment=right;}
}