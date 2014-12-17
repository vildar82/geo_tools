// импорт точек из файла
d_import_cogo_points:dialog{key="top";label="Параметры импорта точек";initial_focus="code";
//:row{
//:text{label="Номер";}
//:edit_box{key="name";value="";edit_width=35;alignment=right;}
//}
//:row{
//:text{label="Описание";}
//:edit_box{key="code";value="";edit_width=35;alignment=right;}
//}
//:row{
//:button{label="[P] номер";key="number-button";fixed_width=true;}
//:button{label="[D] описание";key="code-button";fixed_width=true;}
//}
:column{label="Вставлять координаты точек";
:row{
:column{
:toggle{label="Загруженые";key="upload";value="1";}
:toggle{label="Введенные";key="input";value="1";}
}
:column{
:toggle{label="Вычисленные";key="calculation";value="1";}
:toggle{label="Непонятные";key="unknown";value="1";}
}
}
}
:toggle{label="Дата съемки => Имя слоя";key="layer-name";value="1";}
:popup_list{label="Формат файла";key="format";list="Nikon RAW\nNikon PNEZD (разделение запятыми)\nSokkia SDR33\nLeica GSI";edit_width=30;}
ok_cancel;
}