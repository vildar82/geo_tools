// редактирование вида профиля
profile_dialog_edit_properti_curent_basement:dialog{label="Редактирование вида профиля";
:row{label="Содержимое подвала";
:column{
:list_box{label="Подвал";key="list-podval-str";list="";height=15;width=30;}
}
:column{
:edit_box{label="Название";key="name";value="";}
:edit_box{label="Высота строки в мм";key="h-str";value="";edit_width=5;}
:edit_box{label="Высота текста в мм";key="h-txt";value="";edit_width=5;}
:toggle{label="Номера точек в круге";key="flag-circle";value="0";}
:edit_box{label="Мин угол в градусах";key="min-angle";value="";edit_width=5;}
:popup_list{label="Чисел после запятой";key="dec";list="0\n0.0\n0.00\n0.000";}
:popup_list{label="Уклон измеряется в  ";key="sel-uklon";list="Проценты %\nПромили  \U+2030";}
:popup_list{label="Чисел после запятой в уклоне";key="dec-downgrade";list="0\n0.0\n0.00\n0.000\n0.0000";}
}
}
:column{label="Параметры текущего вида профиля";
:popup_list{label="Текстовый стиль профиля";key="text-style";list="";}
:edit_box{label="Высота текста пересечек в мм";key="h-txt-intersect";value="";edit_width=5;}
:row{
:popup_list{label="Чисел после запятой в выносках";key="accuracy-mleader";list="0\n0.0\n0.00\n0.000";}
:edit_box{label="Начальный пикет";key="picket-first";value="0+0.0";edit_width=5;}
}
}
ok_cancel;
}