// ��������� ���������
d_draw_deflexion_stilt: dialog{label="��������� ������� ��������";//initial_focus="num_1";
:toggle{label="���������� �����";key="chek_num";value="1";}
:edit_box{label="������ �";key="num_1";value="1";edit_width=3;}
:toggle{label="���������� �������� � �����";key="chek_xy";value="1";}
:popup_list{label="������� ���������";key="ei_xy";
list="��\n��\n�";}
:toggle{label="���������� �������� �� �������";key="chek_z";value="1";}
:popup_list{label="������� ���������";key="ei_z";
list="��\n��\n�";}
:popup_list{label="������� ����� �";key="sel_z";
list="���������\n����������";}
ok_cancel;
}