// Настройка пересечек для профиля
set_intersection_lines_type:dialog{label="Настройка пересечек";
:list_box{label="Существующие линии пересечек";key="list-intersection-lines";list="";height=30;width=80;}
:row{
:button{label="Создать";key="new";}
:button{label="Удалить";key="del";}
}
:row{label="Редактирование описания";
:edit_box{key="edit-discription";value="";}
}
ok_cancel;
}