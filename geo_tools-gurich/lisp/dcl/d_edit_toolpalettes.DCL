// ������������� ����� ������ ������������
d_edit_toolpalettes:dialog{label="������������� ����� ������ ������������";
:row{
:list_box{label="�������";key="list-palettes";list="";height=30;width=30;}
:list_box{label="�����������";key="list-tools";list="";height=30;width=30;}
:column{
:image{key="pic";width=11;height=5;color=1;alignment=centered;fixed_width=true;fixed_height=true;}
:edit_box{label="��������";key="name";value="";edit_width=10;alignment=centered;}
:edit_box{label="��������";key="description";value="";edit_width=10;alignment=centered;}
:edit_box{label="�������";key="macro";value="";edit_width=10;alignment=centered;}
:button{label="�����";key="up";fixed_width=true;alignment=centered;}
:button{label="����";key="down";fixed_width=true;alignment=centered;}
}
}
:row{

}
:column{
:button{label="���������";key="save";is_default=true;is_cancel=false;fixed_width=true;alignment=right;}
:ok_button{label="������";is_cancel=true;is_default=false;alignment=right;}
}
}