// формат номера точки в sdr файла при экспорте таблицы
d_export_table_to_sdr:dialog{label="Параметры";
:row{
:column{
:toggle{label="Вкл/Выкл ";key="flag1";value="0";}
:popup_list{key="select1";list="Имя таблицы\n№№ пункта\nНаименование";width=16;}
}
:column{
:toggle{key="flag2";value="0";}
:edit_box{key="symbol1";value="_";edit_width=0.5;}
}
:column{
:toggle{label="Вкл/Выкл ";key="flag3";value="0";}
:popup_list{key="select2";list="Имя таблицы\n№№ пункта\nНаименование";width=16;}
}
:column{
:toggle{key="flag4";value="0";}
:edit_box{key="symbol2";value="_";edit_width=0.5;}
}
:column{
:toggle{label="Вкл/Выкл ";key="flag5";value="0";}
:popup_list{key="select3";list="Имя таблицы\n№№ пункта\nНаименование";width=16;}
}
}
:text{label="";key="text_str";width=45;}
:popup_list{label="Формат";key="format";list="Nicon PNEZD\nSokkia SDR\nLeica GSI";width=50;}
:row{
:button{label="Сохранить";key="save";}
:ok_button{label="Выход";is_cancel=true;}
}
}