(defun analysis_limit_resizing (list_granic 2Dpline / granica p1 p2 Dgranicy new_list_granic)
  (repeat (length list_granic) ; модуль расширения границ занимаемых отметками
    (setq granica (car list_granic))
    (setq list_granic (cdr list_granic))
    (setq p1 (car granica))
    (setq p2 (cadr granica))
    (setq Dgranicy (*(analisis_kol-vo_elevation_in_limit p1 p2 2Dpline)(/ (* 2 (/ 4.0 3.0)) (/ 1000 mg))))
    (setq g1 (-(analysis_centr_limit_be_elevation p1 p2 2Dpline)(/ Dgranicy 2)))
    (setq g2 (+(analysis_centr_limit_be_elevation p1 p2 2Dpline)(/ Dgranicy 2)))
    (setq new_list_granic (append new_list_granic(list (list g1 g2))))
    )
  new_list_granic
  );defun
