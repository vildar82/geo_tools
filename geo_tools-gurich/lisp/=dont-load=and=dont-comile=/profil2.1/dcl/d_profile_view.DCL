// создание редактирование вида профиля
d_profile_view:dialog{label="Создание редактирование вида профиля";
:column{label="Вид профиля";
:popup_list{key="view-basement-name";list="";}
:row{
:button{label="Новый";key="new-view-basement";}
:button{label="Переименовать";key="rename-view-basement";}
:button{label="Удалить";key="delete-view-basement";}
}
}
:row{label="Содержимое подвала";
:column{
:list_box{label="Подвал";key="profile-strings";list="";height=15;width=30;}
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
:toggle{label="Вставить линейку";key="draw-ruler";value="1";}
:edit_box{label="Высота текста заголовков в мм";key="h-title-text";value="";edit_width=5;}
}
:edit_box{label="Ширина блока заголовков подвала в мм";key="width-basement";value="";edit_width=5;}
:edit_box{label="Расстояние от линейки до начала профиля в мм";key="buffer-dist";value="";edit_width=5;}
:popup_list{label="Текстовый стиль профиля";key="text-style";list="";}
:column{label="Строки описания характеристик профиля";
:edit_box{label="Название";key="basement-str-title";value="";edit_width=40;}
:edit_box{label="Подпись перед Вертикальным масштабом";key="basement-str-vertical-scale";value="";edit_width=40;}
:edit_box{label="Подпись перед Горизонтальным масштабом";key="basement-str-horizontal-scale";value="";edit_width=40;}
:edit_box{label="Подпись перед условным горизонтом";key="basement-str-reference-level";value="";edit_width=40;}
:edit_box{label="Высота текста пересечек в мм";key="h-intersection-text";value="";edit_width=5;}
:popup_list{label="Чисел после запятой в выносках";key="accuracy-mleader";list="0\n0.0\n0.00\n0.000";}
}
}
ok_cancel;
}