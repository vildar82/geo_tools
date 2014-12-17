// Каталог координат
tab_koor: dialog{label="Параметры вставки таблицы";initial_focus="n";
:radio_column{label="Таблица";
:radio_button{label="Новая";key="new";value="1";}
:radio_button{label="Продолжить";key="return";value="0";}
}
:spacer{height=0.1;}
:radio_column{label="Подписи точек";
:radio_button{label="Не надо";key="net_podpysi";value="1";}
:radio_button{label="Вставлять";key="ins_podpysi";value="0";}
:radio_button{label="Уже есть";key="est_podpysi";value="0";}
}
//:toggle{label="Вставлять подписи точек";key="pr2";value="0";}
:edit_box{label="Начальный номер";key="n";value="1";edit_width=3;}
ok_cancel;
}