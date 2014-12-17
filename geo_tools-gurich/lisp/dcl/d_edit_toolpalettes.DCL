// Редактировать файлы палитр инструментов
d_edit_toolpalettes:dialog{label="Редактировать файлы палитр инструментов";
:row{
:list_box{label="Палитры";key="list-palettes";list="";height=30;width=30;}
:list_box{label="Инструменты";key="list-tools";list="";height=30;width=30;}
:column{
:image{key="pic";width=11;height=5;color=1;alignment=centered;fixed_width=true;fixed_height=true;}
:edit_box{label="Название";key="name";value="";edit_width=10;alignment=centered;}
:edit_box{label="Описание";key="description";value="";edit_width=10;alignment=centered;}
:edit_box{label="Команда";key="macro";value="";edit_width=10;alignment=centered;}
:button{label="Вверх";key="up";fixed_width=true;alignment=centered;}
:button{label="Вниз";key="down";fixed_width=true;alignment=centered;}
}
}
:row{

}
:column{
:button{label="Сохранить";key="save";is_default=true;is_cancel=false;fixed_width=true;alignment=right;}
:ok_button{label="Отмена";is_cancel=true;is_default=false;alignment=right;}
}
}