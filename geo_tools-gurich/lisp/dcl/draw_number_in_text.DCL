// Нумератор
draw_number_in_text: dialog{label="Параметры нумерации";initial_focus="number-first";
:popup_list{key="mode";list="Создать объекты\nДобавить в сущ. объекты\nЗаменить содержимое\nНумеруем вершины полилинии";}
:popup_list{key="create-type";list="Текст\nМтекст\nМвыноска";}
:popup_list{key="create-mode";list="";}

:column{label="Текст номера";
:edit_box{label="Префикс";key="perfix";value="";edit_width=7;}
:edit_box{label="Начать с";key="number-first";value="1";edit_width=3;}
:edit_box{label="Суффикс";key="suffix";value="";edit_width=7;}
}
:edit_box{label="Высота текста в мм";key="h-text";value="";edit_width=3;}
ok_cancel;
}