// ���� ������ ������� 
import_punkts_optyons:dialog{label="��������� ������� ������� � �������";
:column{
:radio_column{label="��� ������������?";
:radio_button{label="��������� � ������";key="memory";value="0";}
:radio_button{label="�������� � ������";key="file";value="1";}
}
}
:toggle{label="������ ��� ������ � ���� ������";key="flag-type";value="1";}
:toggle{label="������ ��� ��������� ������ � ���� ������";key="flag-year";value="1";}
:toggle{label="������� �� ������ ����� ������";key="draw-new-punkt";value="0";}
:toggle{label="���������� ������� � ����� �����";key="file-name-messege";value="1";}
:edit_box{label="������ ���� �� ��������� �� ���-�� �����(0 - �� ������)";key="kol-vo-str";value="1000";edit_width=5;}
:toggle{label="���������� ��������� ������";key="continue-calc";value="0";}
//:row{label="������������� ����";
//:edit_box{key="path";value="";edit_width=50;}
//:button{label="�����";key="find";}
//}
ok_cancel;
}