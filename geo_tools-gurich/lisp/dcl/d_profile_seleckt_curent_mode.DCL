// выбор текущего режима профиля
d_profile_seleckt_curent_mode:dialog{key="top";label="Выбор текущего профиля";
:radio_column{
:radio_button{label="Новый профиль";key="new";value="1";}
:row{
:popup_list{key="seleckt-list";list="";width=8;}
:button{label="Указать";key="seleckt-button";alignment=right;}
}
:radio_button{label="Старый профиль";key="old";value="0";}
:row{
spacer_1;
:column{
:edit_box{label="Вертикальный масштаб";key="vertical-scale";value="";edit_width=6;alignment=right;}
:edit_box{label="Горизонтальный масштаб";key="horizontal-scale";value="";edit_width=6;alignment=right;}
:edit_box{label="Условный горизонт";key="reference-level";value="";edit_width=6;alignment=right;}
:edit_box{label="Высота текста в мм";key="height-text";value="";edit_width=5;}
:popup_list{label="Чисел после запятой";key="accuracy";list="0\n0.0\n0.00\n0.000";}
:popup_list{label="Уклон измеряется в  ";key="sel-uklon";list="Проценты %\nПромили  \U+2030";}
:popup_list{label="Чисел после запятой в уклоне";key="accuracy-downgrade";list="0\n0.0\n0.00\n0.000\n0.0000";width=7;}
:edit_box{label="Начальный пикет";key="picket-first";value="";edit_width=6;}
}
}
}
ok_cancel_err;
}