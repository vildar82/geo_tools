// Вставка формата заданного размера
d_format_GOST_ISO: dialog{label="Размер рамки";
:popup_list{key="vibor_ramka";list="";}
:row{
:column{
:radio_column{label="Стандарт";
:radio_button{label="ГОСТ";key="typ_GOST";value="0";}
:radio_button{label="ISO";key="typ_ISO";value="0";}
:radio_button{label="Свое";key="typ_USER";value="0";}
}
:toggle{label="Поворот";key="rotate_ramka";value="0";}
}
:radio_column{label="Штамп";
:radio_button{label="ГОСТ";key="shtamp_GOST";value="0";}
:radio_button{label="Большой";key="shtamp_BIG";value="0";}
:radio_button{label="Маленький";key="shtamp_SMAL";value="0";}
:radio_button{label="Нет";key="shtamp_NET";value="0";}
}
}
:toggle{label="Вставка штампа с инв.№";key="ins_smal_shtamp";value="0";}
:toggle{label="Без вспомогательных рамок";key="model-list";value="0";}
ok_cancel;
}
