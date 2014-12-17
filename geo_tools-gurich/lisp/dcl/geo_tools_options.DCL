// база данных реперов 
geo_tools_options:dialog{label="Общие настройки Geo_Tools";
:column{label="Сервер обновления";
:toggle{label="Проверять наличие обновлений при запуске";key="server-check-update-flag";value="0";}
:toggle{label="Автоматически обновлять Geo_Tools";key="server-auto-update-flag";value="0";}
:edit_box{label="Путь папке обновлений";key="geo_tools-server-patch";value="";edit_width=50;}
:spacer{height=0.1;}
}
ok_cancel;
}