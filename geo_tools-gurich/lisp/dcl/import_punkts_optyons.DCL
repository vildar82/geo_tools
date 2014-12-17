// база данных реперов 
import_punkts_optyons:dialog{label="Параметры импорта пунктов в каталог";
:column{
:radio_column{label="Где обрабатывать?";
:radio_button{label="Загружать в память";key="memory";value="0";}
:radio_button{label="Работать с файлом";key="file";value="1";}
}
}
:toggle{label="Искать тип пункта в поле адреса";key="flag-type";value="1";}
:toggle{label="Искать год получения данных в поле адреса";key="flag-year";value="1";}
:toggle{label="Нанести на чертеж новые пункты";key="draw-new-punkt";value="0";}
:toggle{label="Отображать процесс в имени файла";key="file-name-messege";value="1";}
:edit_box{label="Делить файл на несколько по кол-ву строк(0 - не делить)";key="kol-vo-str";value="1000";edit_width=5;}
:toggle{label="Продолжить прерваный импорт";key="continue-calc";value="0";}
//:row{label="Импортируемый файл";
//:edit_box{key="path";value="";edit_width=50;}
//:button{label="Обзор";key="find";}
//}
ok_cancel;
}