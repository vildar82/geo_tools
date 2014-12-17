// база данных реперов 
options_coordinate_katalog:dialog{label="Параметры каталога координат";
:row{label="Типы пунктов";
:button{label="Настроить";key="properties_punkts";fixed_width=true;}
}
:row{label="Файл каталога";
:edit_box{key="path";value="";edit_width=50;}
:button{label="Обзор";key="find";}
}
ok_cancel;
}