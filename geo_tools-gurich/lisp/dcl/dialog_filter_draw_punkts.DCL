// Оформление трассы
dialog_filter_draw_punkts: dialog{label="Фильтрация наносимых пунктов";
:toggle{label="Выключить фильтр";key="filter-mode";value="0";}
:column{label="Использовать пункты";
:toggle{label="Без отметки";key="no-z";value="0";}
:toggle{label="С отметкой";key="yes-z";value="0";}
:toggle{label="Без года";key="no-year";value="0";}
}
:toggle{label="Ограничить годами";key="years-limit";value="0";}
:row{
:edit_box{label="от";key="year-min";value="1900";edit_width=4;}:edit_box{label="до";key="year-max";value="3000";edit_width=4;}
}
:toggle{label="Фильтр по типу пункта";key="tipe-filter";value="0";}
:list_box{key="list-tipe-punkts";list="";height=20;width=5;}
:toggle{label="Выключить пункты этого типа";key="on-off-punkt";value="0";}
ok_cancel;
}