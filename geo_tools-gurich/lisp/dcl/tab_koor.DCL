// ������� ���������
tab_koor: dialog{label="��������� ������� �������";initial_focus="n";
:radio_column{label="�������";
:radio_button{label="�����";key="new";value="1";}
:radio_button{label="����������";key="return";value="0";}
}
:spacer{height=0.1;}
:radio_column{label="������� �����";
:radio_button{label="�� ����";key="net_podpysi";value="1";}
:radio_button{label="���������";key="ins_podpysi";value="0";}
:radio_button{label="��� ����";key="est_podpysi";value="0";}
}
//:toggle{label="��������� ������� �����";key="pr2";value="0";}
:edit_box{label="��������� �����";key="n";value="1";edit_width=3;}
ok_cancel;
}