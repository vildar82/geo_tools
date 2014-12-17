// экспорт точек в файл
d_export_cogo_points:dialog{key="top";label="Параметры импорта точек";initial_focus="description";
:row{
:text{label="Номер";}
:edit_box{key="number";value="";edit_width=35;alignment=right;}
}
:row{
:text{label="Описание";}
:edit_box{key="description";value="";edit_width=35;alignment=right;}
}
:row{
:button{label="[P] номер";key="number-button";fixed_width=true;}
:button{label="[N] имя";key="name-button";fixed_width=true;}
:button{label="[D] описание";key="description-button";fixed_width=true;}
}
:popup_list{label="Формат файла";key="format-file";list="";}
ok_cancel;
}