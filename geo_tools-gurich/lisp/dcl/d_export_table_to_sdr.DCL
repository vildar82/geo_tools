// ������ ������ ����� � sdr ����� ��� �������� �������
d_export_table_to_sdr:dialog{label="���������";
:row{
:column{
:toggle{label="���/���� ";key="flag1";value="0";}
:popup_list{key="select1";list="��� �������\n�� ������\n������������";width=16;}
}
:column{
:toggle{key="flag2";value="0";}
:edit_box{key="symbol1";value="_";edit_width=0.5;}
}
:column{
:toggle{label="���/���� ";key="flag3";value="0";}
:popup_list{key="select2";list="��� �������\n�� ������\n������������";width=16;}
}
:column{
:toggle{key="flag4";value="0";}
:edit_box{key="symbol2";value="_";edit_width=0.5;}
}
:column{
:toggle{label="���/���� ";key="flag5";value="0";}
:popup_list{key="select3";list="��� �������\n�� ������\n������������";width=16;}
}
}
:text{label="";key="text_str";width=45;}
:popup_list{label="������";key="format";list="Nicon PNEZD\nSokkia SDR\nLeica GSI";width=50;}
:row{
:button{label="���������";key="save";}
:ok_button{label="�����";is_cancel=true;}
}
}