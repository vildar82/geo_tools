// Вставить символ
d_add_symbol: dialog{label="Вставить символ";
:radio_column{label="Символ";
:radio_button{label="Градус";key="radio1_0";value="1";}
:radio_button{label="Плюс/минус";key="radio1_1";value="0";}
:radio_button{label="Диаметр";key="radio1_2";value="0";}
:radio_button{label="Сумма";key="radio1_3";value="0";}
:radio_button{label="Квадрат";key="radio1_4";value="0";}
:radio_button{label="Куб";key="radio1_5";value="0";}
:radio_button{label="Подчеркнуть";key="radio1_7";value="0";}
:radio_button{label="Надчеркнуть";key="radio1_8";value="0";}
}
:radio_column{label="Добавить";
:radio_button{label="В начале";key="radio2_0";value="0";}
:radio_button{label="В конце";key="radio2_1";value="1";}
}
ok_cancel;
}