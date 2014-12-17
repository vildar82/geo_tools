// Настройки схемы стыков
d_options_styki: dialog{key="top";label="Настройки схемы стыков";
:column{label="Выноски";
:row{
:radio_column{
:radio_button{label="Ручные";key="manual_label";value="0";}
:radio_button{label="Автомат";key="automatic_label";value="1";}
}
:column{
:spacer{height=1;}
:popup_list{key="napr_label";
list="С\nСВ\nВ\nЮВ\nЮ\nЮЗ\nЗ\nСЗ";value="7";}
}
}
}
:edit_box{label="Высота текста";key="d_htext";value="2.5";edit_width=5;}
:edit_box{label="Размер штриха";key="d_dl_shtriha";value="2";edit_width=5;}
:edit_box{label="Длинна отвода";key="d_dl_otvoda";value="4";edit_width=5;}
:column{label="Линия";
:edit_box{label="Длинна линии";key="d_dl_linii";value="20";edit_width=5;}
:button{label="Вычислить";key="calk";action="(done_dialog 3)";}
}
ok_cancel;
}