// редактор исходных пунктов
d_edit_punkts:dialog{key="top";label="Редактор пунктов";
:row{
:column{
:edit_box{label="Имя";key="name";value="";edit_width=20;}
:popup_list{label="Тип пункта";key="type";list="";}
:edit_box{label="Год";key="year";value="";edit_width=6;}
}
:column{
:edit_box{label="Северная координата";key="N";value="";edit_width=20;}
:edit_box{label="Восточная координата";key="E";value="";edit_width=20;}
:edit_box{label="Отметка";key="Z";value="";edit_width=20;}
}
}
:edit_box{label="Адрес, местоположение";key="address";value="";}
:edit_box{label="Коментарий";key="comment";value="";}
:button{label="Сохранить";key="save";is_default=true;is_cancel=false;fixed_width=true;alignment=right;}
:ok_button{label="Отмена";is_cancel=true;is_default=false;alignment=right;}
}
