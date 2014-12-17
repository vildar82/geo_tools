// Диалог Да-Нет
d_YES_NO: dialog{label="Вопрос";
spacer_1;
:text{label="Вопрос";key="text_str";alignment=centered;}
spacer_1;
:row{
  :ok_button{label="ДА";is_cancel=true;fixed_height=true;}
  :button{label="НЕТ";key="calk";action="(term_dialog)";fixed_height=true;}
  }
  }