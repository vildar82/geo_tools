// ���������� ������
dialog_filter_draw_punkts: dialog{label="���������� ��������� �������";
:toggle{label="��������� ������";key="filter-mode";value="0";}
:column{label="������������ ������";
:toggle{label="��� �������";key="no-z";value="0";}
:toggle{label="� ��������";key="yes-z";value="0";}
:toggle{label="��� ����";key="no-year";value="0";}
}
:toggle{label="���������� ������";key="years-limit";value="0";}
:row{
:edit_box{label="��";key="year-min";value="1900";edit_width=4;}:edit_box{label="��";key="year-max";value="3000";edit_width=4;}
}
:toggle{label="������ �� ���� ������";key="tipe-filter";value="0";}
:list_box{key="list-tipe-punkts";list="";height=20;width=5;}
:toggle{label="��������� ������ ����� ����";key="on-off-punkt";value="0";}
ok_cancel;
}