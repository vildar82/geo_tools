// �������� ������ �������
dialog_create_new_profile:dialog{label="��������� ������ �������";
:edit_box{label="��������";key="profile-name";value="";}
:popup_list{label="��� �������";key="view-basement";list="";}

:edit_box{label="��������� ������������� �������� 1:";key="vertical-scale";value="";edit_width=3;}
:edit_box{label="��������� ��������������� �������� 1:";key="horizontal-scale";value="";edit_width=3;}
:column{label="�������� ��������(��)";
:toggle{label="���� ������� ��";key="auto-reference-level";value="1";}
:edit_box{label="������������ ������� �������";key="max-level";value="";edit_width=5;}
:edit_box{label="��������� �������";key="label-level";value="";edit_width=5;}
:edit_box{label="������� ��";key="reference-level";value="";edit_width=5;}
:edit_box{label="������ ����� ����� ������������ �������";key="up-elevation-buffer";value="";edit_width=5;}
:edit_box{label="������ ����� ��� �� � �";key="elevation-buffer";value="";edit_width=5;}
:spacer{height=0.1;}
}
:spacer{height=0.1;}
:row{label="�����������";
:toggle{label="������� �����������";key="draw-intersection";value="1";}
:button{label="���������";key="set-intersection";}
}
:toggle{label="��������� �����";key="draw-points-name";value="1";}
:toggle{label="�������� ���� ��������";key="draw-angle";value="1";}
:edit_box{label="��������� �����";key="picket-first";value="0+0.0";edit_width=5;}
ok_cancel;
}