// ���� ������ ������� 
d_edit_basa:dialog{label="�������� ����������";
:list_box{key="list-punct";list="";height=20;width=80;}
:row{
:column{
:edit_box{label="���";key="name";value="";}
:popup_list{label="��� ������";key="type";list="";}
:edit_box{label="���";key="year";value="";edit_width=6;}
}
:column{
:edit_box{label="�������� ����������";key="N";value="";edit_width=20;}
:edit_box{label="��������� ����������";key="E";value="";edit_width=20;}
:edit_box{label="�������";key="Z";value="";edit_width=20;}
}
}
:edit_box{label="�����, ��������������";key="address";value="";}
:edit_box{label="����������";key="comment";value="";}
:button{label="���������";key="save";is_default=true;is_cancel=false;fixed_width=true;alignment=right;}
:ok_button{label="������";is_cancel=true;is_default=false;alignment=right;}
}