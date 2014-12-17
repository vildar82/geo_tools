(defun C:profile_create_new_profile (/ pline curent-block-profile data-list-dialog insert-point)
  ; создание нового подвала профиля на основе "вида профиля"
  (geo_tools-help "profile_create_new_profile")
  (setq pline (car (entsel "\nВыберите полилинию трассы: ")))
  (cond
    ((eq pline nil)
     (alert "Ничего не выбрано!"))
    ((not(or(eq (cdr(assoc 0 (entget pline))) "POLYLINE")(eq (cdr(assoc 0 (entget pline))) "LWPOLYLINE")))
     (alert "Выбрана не полилиния!"))
    (T
     (setq data-list-dialog(profile-dialog-create-new-profile))
     (if data-list-dialog
       (progn
	 (setq insert-point (getpoint"\nУкажи точку вставки профиля: "))
	 (if insert-point
	   (progn
	     (setq curent-block-profile(profile-draw-block-by-view-basement insert-point data-list-dialog))
	     (if(eq (cdr(assoc 0 (entget pline))) "LWPOLYLINE")
	       (vlax-ldata-put curent-block-profile "coor-trace" (edit-list-del-duble-points(list-coordinates-lwpolyline pline)))
	       (vlax-ldata-put curent-block-profile "coor-trace" (edit-list-del-duble-points(list-coordinates-polyline pline)))
	       )
	     (profile-draw-gorizontal-lines curent-block-profile)
	     (if (= (get-item-by-name-in-list data-list-dialog "draw-points-name") 1); чертить номера точек в подвале
	       (profile-draw-number-points curent-block-profile)
	       )
	     (if (= (get-item-by-name-in-list data-list-dialog "draw-angle") 1); чертить углы поворота трассы в подвале
	       (profile-draw-angle-trace curent-block-profile)
	       )
	     (if (eq (get-item-by-name-in-list data-list-dialog "draw-intersection") 1); чертить пересечки трассы комуникациями в профиле
	       (profile-draw-intersection-lines curent-block-profile)
	       )
	     );progn
	   );if
	 );progn
       );if
     )
    );cond
  (princ)
  )

