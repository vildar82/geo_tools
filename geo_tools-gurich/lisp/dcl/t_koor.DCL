// Каталог координат
t_koor: dialog{label="Параметры вставки таблицы";initial_focus="n";
  :toggle{label="Вставить шапку";key="pr1";value="0";}
  :toggle{label="Вставлять подписи точек";key="pr2";value="0";}
  :edit_box{label="Начальный номер";key="n";value="1";edit_width=3;}
  ok_cancel;
  }