// создание редактирование вида профиля
d_profile_str_edit:dialog{label="Редактирование строки подвала";initial_focus="name-str";
:edit_box{label="Название";key="name-str";value="";}
:edit_box{label="Высота строки в мм";key="h-str";value="";edit_width=5;}
:edit_box{label="Высота текста в мм";key="h-text";value="";edit_width=5;}
:toggle{label="Номера точек в круге";key="draw-circle";value="0";}
:edit_box{label="Мин угол в градусах";key="min-angle";value="";edit_width=5;}
:popup_list{label="Чисел после запятой";key="accuracy-text";list="0\n0.0\n0.00\n0.000";}
:popup_list{label="Уклон измеряется в  ";key="sel-units-downgrade";list="Проценты %\nПромили  \U+2030";}
:popup_list{label="Чисел после запятой в уклоне";key="accuracy-downgrade";list="0\n0.0\n0.00\n0.000\n0.0000";}
ok_cancel;
}