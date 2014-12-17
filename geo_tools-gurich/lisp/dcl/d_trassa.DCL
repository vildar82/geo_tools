// Оформление трассы
d_trassa: dialog{label="Параметры оформления трассы";//initial_focus="n";

:column{label="Пикетаж";
  :toggle{label="Нанести пикетаж";key="draw-picket";value="0";}
  :popup_list{label="Точность пикетов";key="accuracy-picket";list="0\n0.0\n0.00\n0.000";value="0";}
  :toggle{label="Первый и последний пикет";key="draw-start-end-picket";value="0";}
  }
  :column{label="Углы поворота (УП)";
  :toggle{label="Вставлять подписи УП";key="draw-mleader";value="0";}
  :popup_list{label="Точность углов";key="accuracy-angle";list="0d\n0d00'\n0d00'00\"";value="0";}
  :edit_box{label="Префикс угла";key="perfix-angle";value="";edit_width=30;}
  :popup_list{label="Точность пикетов";key="accuracy-picket-angle";list="0\n0.0\n0.00\n0.000";value="0";}
  :toggle{label="Первый и последний пикет";key="draw-start-end-picket-angle";value="0";}
  :edit_box{label="Минимальный отображаемый угол";key="min-angle";value="00d00'00\"";edit_width=7;}
  }
  :column{label="Координаты";
  :toggle{label="Добавить координаты";key="draw-coor";value="0";}
  :popup_list{label="Чисел перед запятой";key="numbers-coor";list="Без изменений\n123\n1234\n12345\n123456\n1234567\n12345678";value="0";}
  :popup_list{label="Точность координат";key="accuracy-coor";list="0\n0.0\n0.00\n0.000";value="0";}
  }
  :row{
  :edit_box{label="Начальный пикет трассы";key="picket-first";value="0";edit_width=8;}
  :text{label="м";}
  }
    :row{
  :edit_box{label="Толщина полилинии в плане";key="width-poly";value="0.3";edit_width=5;}
  :text{label="мм";}
  }
  
  :toggle{label="Удалить вспомогательную 2Dполилинию";key="del-bak-2poly";value="0";}
  ok_cancel;
  }