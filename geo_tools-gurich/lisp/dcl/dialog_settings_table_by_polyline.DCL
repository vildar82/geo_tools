// Создание нового профиля
dialog_settings_table_by_polyline:dialog{label="Настройки таблицы координат";
:toggle{label="Искать связаные с вершиной мвыноски";key="mleader-enable";value="1";}
:row{
:toggle{label="Перфикс для круглых мвыноскам";key="add-perfix-to-mleader";value="1";}
:edit_box{label="Префикс";key="perfix";value="т.";edit_width=3;}
}
:toggle{label="Продолжать нумерацию при добавлении в сущ таблицу";key="auto-numering";value="1";}
:row{
:edit_box{label="Начинать нумерацию с";key="first-number";value="1";edit_width=3;}
:popup_list{label="Точность координат";key="accuracy-coordinate";list="0\n0.0\n0.00\n0.000";edit_width=5;}
}
:toggle{label="Добавить колонку углов";key="angle-column";value="0";}
ok_cancel;
}