// ��������� ����� ������
d_options_styki: dialog{key="top";label="��������� ����� ������";
:column{label="�������";
:row{
:radio_column{
:radio_button{label="������";key="manual_label";value="0";}
:radio_button{label="�������";key="automatic_label";value="1";}
}
:column{
:spacer{height=1;}
:popup_list{key="napr_label";
list="�\n��\n�\n��\n�\n��\n�\n��";value="7";}
}
}
}
:edit_box{label="������ ������";key="d_htext";value="2.5";edit_width=5;}
:edit_box{label="������ ������";key="d_dl_shtriha";value="2";edit_width=5;}
:edit_box{label="������ ������";key="d_dl_otvoda";value="4";edit_width=5;}
:column{label="�����";
:edit_box{label="������ �����";key="d_dl_linii";value="20";edit_width=5;}
:button{label="���������";key="calk";action="(done_dialog 3)";}
}
ok_cancel;
}