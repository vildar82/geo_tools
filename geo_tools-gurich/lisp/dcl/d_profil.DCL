// ���������� ������� �� 3D���������
d_profil: dialog{label="��������� �������� �������";
:row{
:column{
:toggle{label="�������� ������";key="ins_podval";value="0";}
:edit_box{label="������ ������� � (�)";key="0plus";value="0";edit_width=2;}
:toggle{label="��������� ��� �������";key="chek_v_pr";value="0";}
:popup_list{key="sel_v_pr";
list="����������� � ������\n����������� � ���\n����������\n�����������\n����������\n����������� � ������ � ��������";}
}
:column{
:edit_box{label="��������� ������������� �������� 1:";key="mv";value="100";edit_width=3;}
:edit_box{label="��������� ��������������� �������� 1:";key="mg";value="500";edit_width=3;}
:column{
:row{
:text{label="������� ��������� ���������";key="text_usl_z";}
:edit_box{key="usl_z";value="100";edit_width=5;}
}
}
:spacer{height=2;}
}
}
:row{
:column{
:column{label="����� �������";
:toggle{label="������ �� ������";key="on_view";value="1";}
:edit_box{label="X:";key="xp";value="0.000";}
:edit_box{label="Y:";key="yp";value="0.000";}
:edit_box{label="Z:";key="zp";value="0.000";}
:spacer{height=0.1;}
}
:spacer{height=2.5;}
:toggle{label="��������� �����";key="draw_points";value="0";}
}
:column{
:column{label="������� �������";
:toggle{label="�������� �������";key="ins_z";value="0";}
:popup_list{key="sel_str_z";
list="����������� ������� ����� (�����)\n������� ����� ������\n������ ���� ������\n������� ��� �����";}
:row{
:spacer{width=1;}
:toggle{alignment=right;label="����������� ������������";key="optim_z";value="0";}
}
:spacer{height=0.1;}
}
:toggle{label="�������� �����";key="draw_line";value="0";}
:toggle{label="�������� ���� ��������";key="draw_ugli";value="0";}
:row{
:spacer{height=0.1;}
:edit_box{label="��������� ������ ������ ��";key="start";value="0";edit_width=6;}
:text{label="�";}
}
:toggle{label="��������� ����������";key="draw_rasst";value="0";}
:toggle{label="������� �����������";key="draw_peres";value="0";}

}
}
:spacer{height=0.1;}
:column{label="�����/������";
:row{
:toggle{label="������� �����/������";key="draw_uklon";value="0";}
:popup_list{label="����� ���������� �";key="sel_uklon";
list="�������� %\n�������  \U+2030";}
}
:spacer{height=0.1;}
}
ok_cancel;
}