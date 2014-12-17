// Запрос текстовой строки
d_get_string:dialog{key="top";label="";initial_focus="text-str";
:edit_box{key="text-str";value="";edit_width=40;}
:row{
spacer_1;
:button{label="OK";key="ok";is_default=true;is_cancel=false;}
:ok_button{label="Отмена";is_cancel=true;is_default=false;}
}
}