// ������� ����� � ����
d_export_cogo_points:dialog{key="top";label="��������� ������� �����";initial_focus="description";
:row{
:text{label="�����";}
:edit_box{key="number";value="";edit_width=35;alignment=right;}
}
:row{
:text{label="��������";}
:edit_box{key="description";value="";edit_width=35;alignment=right;}
}
:row{
:button{label="[P] �����";key="number-button";fixed_width=true;}
:button{label="[N] ���";key="name-button";fixed_width=true;}
:button{label="[D] ��������";key="description-button";fixed_width=true;}
}
:popup_list{label="������ �����";key="format-file";list="";}
ok_cancel;
}