// Оформление построеного профиля
d_2d_profil: dialog{label="Параметры оформления профиля";
:row{
:column{
:toggle{label="Вставить подвал";key="ins_podval";value="0";}
:toggle{label="Учитывать вид профиля";key="chek_v_pr";value="0";}
:spacer{height=1;}
:popup_list{key="sel_v_pr";
list="Теплотрасса в канале\nТеплотрасса в ППУ\nВодопровод\nКанализация\nГазопровод\nТеплотрасса в канале с дренажом";}
}
:column{
:edit_box{label="Числитель вертикального масштаба 1:";key="mv";value="100";edit_width=3;}
:edit_box{label="Числитель горизонтального масштаба 1:";key="mg";value="500";edit_width=3;}
:column{
:row{
:text{label="Отметка условного горизонта";key="text_usl_z";}
:edit_box{key="usl_z";value="100";edit_width=5;}
}
}
:spacer{height=2;}
}
}
:row{
:column{label="Точка на условном горизонте";
:toggle{label="Задать на экране";key="on_view";value="1";}
:edit_box{label="X:";key="xp";value="0.000";}
:edit_box{label="Y:";key="yp";value="0.000";}
:edit_box{label="Z:";key="zp";value="0.000";}
:spacer{height=0.1;}
}
:column{
:column{label="Вставка отметок";
:toggle{label="Вставить отметки";key="ins_z";value="0";}
:popup_list{key="sel_str_z";
list="Фактические отметки земли (люков)\nОтметка верха канала\nОметка пола канала\nОтметка оси трубы";}
:row{
:spacer{width=1;}
:toggle{alignment=right;label="Оптимизация расположения";key="optim_z";value="0";}
}
:spacer{height=0.1;}
}
:toggle{label="Рисовать линии";key="draw_line";value="0";}
:toggle{label="Подписать расстояние";key="draw_rasst";value="0";}
}
}
:spacer{height=0.1;}
:column{label="Уклон/длинна";
:row{
:toggle{label="Нанести уклон/длинну";key="draw_uklon";value="0";}
:popup_list{label="Уклон измеряется в";key="sel_uklon";
list="Проценты %\nПромили  \U+2030";}
}
:spacer{height=0.1;}
}
ok_cancel;
}