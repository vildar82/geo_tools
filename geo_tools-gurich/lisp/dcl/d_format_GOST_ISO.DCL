// ������� ������� ��������� �������
d_format_GOST_ISO: dialog{label="������ �����";
:popup_list{key="vibor_ramka";list="";}
:row{
:column{
:radio_column{label="��������";
:radio_button{label="����";key="typ_GOST";value="0";}
:radio_button{label="ISO";key="typ_ISO";value="0";}
:radio_button{label="����";key="typ_USER";value="0";}
}
:toggle{label="�������";key="rotate_ramka";value="0";}
}
:radio_column{label="�����";
:radio_button{label="����";key="shtamp_GOST";value="0";}
:radio_button{label="�������";key="shtamp_BIG";value="0";}
:radio_button{label="���������";key="shtamp_SMAL";value="0";}
:radio_button{label="���";key="shtamp_NET";value="0";}
}
}
:toggle{label="������� ������ � ���.�";key="ins_smal_shtamp";value="0";}
:toggle{label="��� ��������������� �����";key="model-list";value="0";}
ok_cancel;
}
