// создание редактирование вида профиля
d_profile_view:dialog{label="Создание редактирование вида профиля";
:column{label="Вид профиля";
:popup_list{key="profile-name";list="";}
:row{
:button{label="Новый";key="new";}
:button{label="Переименовать";key="rename";}
:button{label="Удалить";key="delete";}
}
}
:row{label="Содержимое подвала";
:column{
:list_box{label="Подвал";key="list-podval-str";list="";height=15;width=30;}
}
:column{
spacer_1;
:button{label="<< Добавить";key="add";}
:button{label="Вверх";key="up";}
:button{label="Вниз";key="down";}
:button{label="< Редактировать";key="edit";}
:button{label="Удалить";key="del";}
spacer_1;
:button{label="Редактировать >";key="edit_standart_str";fixed_height=true;}

}
:column{
:list_box{label="Стандартные строки";key="list-standart-str";list="";height=15;width=30;}
}
}
:column{label="Параметры текущего вида профиля";
:row{
:toggle{label="Вставить линейку";key="flag_ruler";value="1";}
:edit_box{label="Высота текста заголовков в мм";key="h-txt";value="";edit_width=5;}
}
:edit_box{label="Ширина блока заголовков подвала в мм";key="d-basement";value="";edit_width=5;}
:edit_box{label="Расстояние от линейки до начала профиля в мм";key="buffer-dist";value="";edit_width=5;}
:popup_list{label="Текстовый стиль профиля";key="text-style";list="";}
:column{label="Строки описания характеристик профиля";
:edit_box{label="Название";key="str4";value="";edit_width=40;}
:edit_box{label="Подпись перед вертикальным масштабом";key="str3";value="";edit_width=40;}
:edit_box{label="Подпись перед Горизонтальным масштабом";key="str2";value="";edit_width=40;}
:edit_box{label="Подпись перед условным горизонтом";key="str1";value="";edit_width=40;}
:edit_box{label="Высота текста пересечек в мм";key="h-txt-intersect";value="";edit_width=5;}
:popup_list{label="Чисел после запятой в выносках";key="accuracy-mleader";list="0\n0.0\n0.00\n0.000";}
}
}
ok_cancel;
}