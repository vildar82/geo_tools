// ����� �������� ������ �������
d_profile_seleckt_curent_mode:dialog{key="top";label="����� �������� �������";
:radio_column{
:radio_button{label="����� �������";key="new";value="1";}
:row{
:popup_list{key="seleckt-list";list="";width=8;}
:button{label="�������";key="seleckt-button";alignment=right;}
}
:radio_button{label="������ �������";key="old";value="0";}
:row{
spacer_1;
:column{
:edit_box{label="������������ �������";key="vertical-scale";value="";edit_width=6;alignment=right;}
:edit_box{label="�������������� �������";key="horizontal-scale";value="";edit_width=6;alignment=right;}
:edit_box{label="�������� ��������";key="reference-level";value="";edit_width=6;alignment=right;}
:edit_box{label="������ ������ � ��";key="height-text";value="";edit_width=5;}
:popup_list{label="����� ����� �������";key="accuracy";list="0\n0.0\n0.00\n0.000";}
:popup_list{label="����� ���������� �  ";key="sel-uklon";list="�������� %\n�������  \U+2030";}
:popup_list{label="����� ����� ������� � ������";key="accuracy-downgrade";list="0\n0.0\n0.00\n0.000\n0.0000";width=7;}
:edit_box{label="��������� �����";key="picket-first";value="";edit_width=6;}
}
}
}
ok_cancel_err;
}