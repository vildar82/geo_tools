// Подписать смещенние
d_draw_deflexion_stilt: dialog{label="Параметры подписи смещений";//initial_focus="num_1";
:toggle{label="Показывать номер";key="chek_num";value="1";}
:edit_box{label="Начать с";key="num_1";value="1";edit_width=3;}
:toggle{label="Показывать смещение в плане";key="chek_xy";value="1";}
:popup_list{label="Еденицы измерения";key="ei_xy";
list="мм\nсм\nм";}
:toggle{label="Показывать смещение по отметке";key="chek_z";value="1";}
:popup_list{label="Еденицы измерения";key="ei_z";
list="мм\nсм\nм";}
:popup_list{label="Отметку брать с";key="sel_z";
list="указателя\nклавиатуры";}
ok_cancel;
}