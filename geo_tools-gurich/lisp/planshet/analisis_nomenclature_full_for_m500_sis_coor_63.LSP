(defun analisis_nomenclature_full_for_m500_sis_coor_63 (t1 / ty tx tystr txstr nam1 nam2 nam3)
(setq tx (nth 0 t1)); определ€ю число Y
(setq ty (nth 1 t1)); определ€ю число x
(setq tystr (rtos ty 2 0))
(setq txstr (rtos tx 2 0))
(setq nam1 (substr tystr 3 2))
(setq nam2 (substr txstr 3 2))
(setq nam3 (analisis_nomenclature_3ch_for_m500_sis_coor_63_1 txstr tystr))
(strcat "+" nam1 "+" nam2 ";" nam3)
  ); end defun analisis_nomenclature_full_for_m500_sis_coor_oms