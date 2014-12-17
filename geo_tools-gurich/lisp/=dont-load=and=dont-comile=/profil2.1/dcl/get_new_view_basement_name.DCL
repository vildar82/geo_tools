// Запрос имени нового вида профиля
get_new_view_basement_name:dialog{key="top";label="";initial_focus="text-str";
:edit_box{key="new-view-basement-name";value="";edit_width=40;}
:toggle{label="Скопировать с текущего";key="copy-curent-basement-profile";value="1";}
:row{
spacer_1;
:button{label="OK";key="ok";is_default=true;is_cancel=false;}
:ok_button{label="Отмена";is_cancel=true;is_default=false;}
}
}