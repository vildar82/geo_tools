// Создание нового профиля
dialog_create_new_profile:dialog{label="Параметры нового профиля";
:edit_box{label="Название";key="profile-name";value="";}
:popup_list{label="Вид профиля";key="view-basement";list="";}

:edit_box{label="Числитель вертикального масштаба 1:";key="vertical-scale";value="";edit_width=3;}
:edit_box{label="Числитель горизонтального масштаба 1:";key="horizontal-scale";value="";edit_width=3;}
:column{label="Условный горизонт(УГ)";
:toggle{label="Авто отметка УГ";key="auto-reference-level";value="1";}
:edit_box{label="Максимальная отметка профиля";key="max-level";value="";edit_width=5;}
:edit_box{label="Подписать отметку";key="label-level";value="";edit_width=5;}
:edit_box{label="Отметка УГ";key="reference-level";value="";edit_width=5;}
:edit_box{label="Чистое место после максимальной отметки";key="up-elevation-buffer";value="";edit_width=5;}
:edit_box{label="Чистое место над УГ в м";key="elevation-buffer";value="";edit_width=5;}
:spacer{height=0.1;}
}
:spacer{height=0.1;}
:row{label="Пересечения";
:toggle{label="Нанести пересечения";key="draw-intersection";value="1";}
:button{label="Настроить";key="set-intersection";}
}
:toggle{label="Подписать точки";key="draw-points-name";value="1";}
:toggle{label="Наносить углы поворота";key="draw-angle";value="1";}
:edit_box{label="Начальный пикет";key="picket-first";value="0+0.0";edit_width=5;}
ok_cancel;
}