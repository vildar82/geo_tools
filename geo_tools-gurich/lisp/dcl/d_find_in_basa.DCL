// поиск в базе исходных пунктов
d_find_in_basa:dialog{label="Найти";
:edit_box{label="Имя";key="name";value="";edit_width=20;}
:row{
:popup_list{label="Тип пункта";key="type";list="не важно\nне указан\nст.репер\nпирамида\nнадстройка";}
:edit_box{label="Год";key="year";value="";edit_width=6;}
}
:row{label="Координаты";
:column{
:edit_box{label="Северная координата";key="N";value="";edit_width=20;}
:edit_box{label="Восточная координата";key="E";value="";edit_width=20;}
:edit_box{label="Отметка";key="Z";value="";edit_width=20;}
:spacer{height=0.1;}
}
:column{
:edit_box{label="Точность координат";key="accuracy";value="";edit_width=2;}
:text{label="(в радиусе указаных)";}
:spacer{height=1.5;}
}
}
:edit_box{label="Адрес, местоположение";key="address";value="";edit_width=40;}
:edit_box{label="Коментарий";key="comment";value="";}
:button{label="Искать";key="save";is_default=true;is_cancel=false;fixed_width=true;alignment=right;}
:ok_button{label="Отмена";is_cancel=true;is_default=false;alignment=right;}
}