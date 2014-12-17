// Подпись горизонталей
dialog_draw_elevation_contour_line: dialog{label="Параметры подписи горизонталей";initial_focus="number-first";
:popup_list{label="Запрашивать отметку";key="input-mode";list="Только когда 0\nВсегда\nНикогда";}
:toggle{label="Умная точность отметки горизонтали";key="smart-accuracy-elevation";value="1";}
:popup_list{label="Точность отметки";key="accuracy-elevation";list="0\n0.0\n0.00\n0.000";}
ok_cancel;
}