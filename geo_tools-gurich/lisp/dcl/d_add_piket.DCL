// Добавить пикет на трассу
d_add_piket: dialog{label="Параметры оформления доп. подписи";initial_focus="start";

:popup_list{label="Точность пикетов";key="accuracy-picket-angle";list="0\n0.0\n0.00\n0.000";value="0";}
  :toggle{label="Добавить координаты";key="draw-coor";value="0";}
  :column{label="Координаты";
  :popup_list{label="Чисел перед запятой";key="numbers-coor";
  list="Без изменений\n123\n1234\n12345\n123456\n1234567\n12345678";value="0";}
  :popup_list{label="Чисел после запятой";key="accuracy-coor";
  list="0\n0.0\n0.00\n0.000";value="2";}
  }
  :row{
  :edit_box{label="Начальный пикет трассы";key="picket-first";value="0";edit_width=6;}
  :text{label="м";}
  }
  ok_cancel;
  }