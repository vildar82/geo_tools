// редактор параметров исходных пунктов
d_properties_punkts_of_katalog:dialog{key="top";label="Редактор типов пунктов";
:list_box{key="list-punct";list="";height=20;width=80;}
:edit_box{label="Тип пункта";key="name";value="";}
:popup_list{label="Блок вставки";key="block-name";list="";}
:row{
:button{label="Создать";key="new";}
:button{label="Удалить";key="del";}
}
:row{
:button{label="Сохранить";key="save";is_default=true;is_cancel=false;alignment=right;}
:ok_button{label="Отмена";is_cancel=true;is_default=false;alignment=right;}
}
}