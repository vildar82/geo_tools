// ������ ����� �� �����
d_import_cogo_points:dialog{key="top";label="��������� ������� �����";initial_focus="code";
//:row{
//:text{label="�����";}
//:edit_box{key="name";value="";edit_width=35;alignment=right;}
//}
//:row{
//:text{label="��������";}
//:edit_box{key="code";value="";edit_width=35;alignment=right;}
//}
//:row{
//:button{label="[P] �����";key="number-button";fixed_width=true;}
//:button{label="[D] ��������";key="code-button";fixed_width=true;}
//}
:column{label="��������� ���������� �����";
:row{
:column{
:toggle{label="����������";key="upload";value="1";}
:toggle{label="���������";key="input";value="1";}
}
:column{
:toggle{label="�����������";key="calculation";value="1";}
:toggle{label="����������";key="unknown";value="1";}
}
}
}
:toggle{label="���� ������ => ��� ����";key="layer-name";value="1";}
:popup_list{label="������ �����";key="format";list="Nikon RAW\nNikon PNEZD (���������� ��������)\nSokkia SDR33\nLeica GSI";edit_width=30;}
ok_cancel;
}