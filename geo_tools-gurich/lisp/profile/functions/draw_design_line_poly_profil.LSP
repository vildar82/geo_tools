(defun C:draw_design_line_poly_profil ; ��������� ��� ���������� �������
       (/ t1 t2 t3 p p1 p2 p3 n pl_prof t0 ht styl z tp dcl_id a b g tt ttt
	q_xyz
	q_x
	q_y
	q_z
	q_ins_podval   ; �������� ������
	q_ins_z        ; �������� �������
	q_sel_str_z    ; ����� ������ ������� �������
	q_sel_v_pr     ; ����� ���� �������
	q_draw_uklon   ; �������� �����
	q_draw_line    ; �������� �����
	q_draw_rasst   ; �������� ����������
	q_draw_ugli    ; �������� ���� ��������
	q_chek_v_pr    ; ��������� ���� �������
	q_sel_uklon    ; ����� ������ ������
	sp_z
	t1_draw_rasst
	t2_draw_rasst
	t1_draw_ugli
	t2_draw_ugli
	t1_ins_z
	t2_ins_z
	tp1
	tp2
	dist
	pro!
	n_00
	min_z
	max_z
	sp_z
	usl_z_v
	min_z
	max_z
	mid_z
	list_granic
	q_optim_z
	)
(geo_tools-help "draw_design_line_poly_profil")
  (setq pline (car (entsel "\n�������� 2D ��������� �������: ")))
  (cond
    ((eq pline nil)
     (alert "������ �� �������!"))
    ((not(eq (cdr(assoc 0 (entget pline))) "LWPOLYLINE"))
     (alert "������� �� 2D ���������!"))
    ((eq (cdr(assoc 0 (entget pline))) "LWPOLYLINE")
     (setq 2Dpline pline)
     (setq spisokv (extract_coord_lwpolyline (entget pline)))
     (setq n (length spisokv))
     (setq mv (file_dat_r_g_w "r" "mv" nil))
     (setq mg (file_dat_r_g_w "r" "mg" nil))
     (setq usl_z (file_dat_r_g_w "r" "usl_z" nil))
     (setq dcl_id (geo-tools-load-dialog "d_2d_profil"))
     (new_dialog "d_2d_profil" dcl_id)
     (mode_tile "sel_v_pr" 1)
     (mode_tile "sel_str_z" 1)
     (mode_tile "sel_uklon" 1)
     (mode_tile "xp" 1)
     (mode_tile "yp" 1)
     (mode_tile "zp" 1)
     (mode_tile "optim_z" 1)
     (if (/= t1! nil)
       (progn
	 (setq q_x (rtos(nth 0 t1!)2 3))
	 (setq q_y (rtos(nth 1 t1!)2 3))
	 (setq q_z (rtos(nth 2 t1!)2 3))
	 )
       (progn
	 (setq q_x "0.000")
	 (setq q_y "0.000")
	 (setq q_z "0.000")
	 )
       )
     (set_tile "xp" q_x)
     (set_tile "yp" q_y)
     (set_tile "zp" q_z)
     (setq q_xyz 1)
     (setq q_sel_v_pr 0)
     (setq q_ins_z 0)
     (setq q_draw_uklon 0)
     (setq q_draw_line 0)
     (setq q_draw_ugli 0)
     (setq q_draw_rasst 0)
     (setq q_sel_str_z 0)
     (setq q_chek_v_pr 0)
     (setq q_ins_podval 0)
     (setq q_draw_rasst 0)
     (setq q_sel_uklon 0)
     (setq q_optim_z 0)
     (setq q_start 0)
     (setq 0plus 0)
     (set_tile "mv" (rtos mv 2 0))
     (set_tile "mg" (rtos mg 2 0))
     (set_tile "usl_z" (rtos usl_z 2 2))
     (action_tile "mv" "(setq mv (get_tile \"mv\"))")
     (action_tile "mg" "(setq mg (get_tile \"mg\"))")
     (action_tile "usl_z" "(setq usl_z (get_tile \"usl_z\"))")
     (action_tile "xp" "(setq q_x (get_tile \"xp\"))")
     (action_tile "yp" "(setq q_y (get_tile \"yp\"))")
     (action_tile "zp" "(setq q_z (get_tile \"zp\"))")
     (action_tile "on_view" ; ������ ����� �������
       "(if
    (eq q_xyz 1)
    (progn
    (mode_tile \"xp\" 0)
    (mode_tile \"yp\" 0)
    (mode_tile \"zp\" 0)
    (setq q_xyz 0)
    )
    (progn
    (mode_tile \"xp\" 1)
    (mode_tile \"yp\" 1)
    (mode_tile \"zp\" 1)
    (setq q_xyz 1)
    )
    )"
       )
     (action_tile "chek_v_pr"; ��������� ��� �������
       "(if (= q_chek_v_pr 0)
    (progn
    (mode_tile \"sel_v_pr\" 0)
    (mode_tile \"sel_uklon\" 1)
    (if (= q_ins_z 1) (mode_tile \"sel_str_z\" 0))
    (if (= q_draw_uklon 1) (mode_tile \"sel_uklon\" 1))
    (if (= q_sel_v_pr 3) (mode_tile \"draw_rasst\" 1)(mode_tile \"draw_rasst\" 0))
    (setq q_chek_v_pr 1)
    )
    (progn
    (mode_tile \"sel_v_pr\" 1)
    (mode_tile \"sel_uklon\" 0)
    (mode_tile \"sel_str_z\" 1)
    (if (= q_draw_uklon 1) (mode_tile \"sel_uklon\" 0)(mode_tile \"sel_uklon\" 1))
    (set_tile \"ins_podval\" \"0\")
    (setq q_ins_podval 0)
    (set_tile \"text_usl_z\" \"������� ��������� ���������\")
    (set_tile \"usl_z\" (rtos usl_z 2 2))
    (mode_tile \"draw_rasst\" 0)
    (setq q_chek_v_pr 0)
    )
    )"
       )
     (action_tile "ins_z"; �������� �������
       "(if (= q_chek_v_pr 1)
    (if (= q_ins_z 0)
    (progn
    (mode_tile \"sel_str_z\" 0)
    (setq q_ins_z 1)
    (mode_tile \"optim_z\" 0)
    )
    (progn
    (mode_tile \"sel_str_z\" 1)
    (setq q_ins_z 0)
    (mode_tile \"optim_z\" 1)
    )
    )
    (if (= q_ins_z 0)
    (progn
    (setq q_ins_z 1)
    (mode_tile \"optim_z\" 0)
    )
    (progn
    (setq q_ins_z 0)
    (mode_tile \"optim_z\" 1)
    )
    )
    )"
       )
     (action_tile "draw_uklon"; �������� �����
       "(if (/= q_chek_v_pr 1)
    (if (= q_draw_uklon 0)
    (progn
    (mode_tile \"sel_uklon\" 0)
    (setq q_draw_uklon 1)
    )
    (progn
    (mode_tile \"sel_uklon\" 1)
    (setq q_draw_uklon 0)
    )
    )
    (if (= q_draw_uklon 0)
    (setq q_draw_uklon 1)
    (setq q_draw_uklon 0)
    )
    )"
       )
     (action_tile "ins_podval"; �������� ������
       "(if (= q_chek_v_pr 0)
    (progn
    (set_tile \"chek_v_pr\" \"1\")
    (setq q_chek_v_pr 1)
    (mode_tile \"sel_v_pr\" 0)
    (if (= q_ins_z 1) (mode_tile \"sel_str_z\" 0))
    (if (= q_draw_uklon 1) (mode_tile \"sel_uklon\" 1))
    )
    )
    (if (= q_ins_podval 0)
    (progn
    (if (= q_sel_v_pr 3) (mode_tile \"draw_rasst\" 1)(mode_tile \"draw_rasst\" 0))
    (setq q_ins_podval 1)
    )
    (progn
    (set_tile \"text_usl_z\" \"������� ��������� ���������\")
    (set_tile \"usl_z\" (rtos usl_z 2 2))
    (setq q_ins_podval 0)
    )
    )"
       )
     (action_tile "sel_v_pr"; ����� ���� ������� ������������ ������ �������
       "(cond
    ((= $value \"0\")
    (start_list \"sel_str_z\")
    (mapcar 'add_list (list \"����������� ������� ����� (�����)\" \"������� ����� ������\" \"������ ���� ������\" \"������� ��� �����\"))
    (end_list)
    (mode_tile \"draw_rasst\" 0)
    (setq q_sel_v_pr 0)
    )
   ((= $value \"1\")
    (start_list \"sel_str_z\")
    (mapcar 'add_list (list \"����������� ������� ����� (�����)\" \"������� ����� �����\" \"������ ���� �����\" \"������� ��� �����\"))
    (end_list)
    (mode_tile \"draw_rasst\" 0)
    (setq q_sel_v_pr 1)
    )
    ((= $value \"2\")
    (start_list \"sel_str_z\")
    (mapcar 'add_list (list \"����������� ������� �����\" \"��������� ������� �����\" \"������� ����� �����\"))
    (end_list)
    (mode_tile \"draw_rasst\" 0)
    (setq q_sel_v_pr 2)
    )
    ((= $value \"3\")
    (start_list \"sel_str_z\")
    (mapcar 'add_list (list \"����������� ������� �����\" \"��������� ������� �����\" \"������ �������� ��������\" \"������� ������ ����\"))
    (end_list)
    (mode_tile \"draw_rasst\" 1)
    (setq q_draw_rasst 0)
    (set_tile \"draw_rasst\" \"0\")
    (setq q_sel_v_pr 3)
    )
    ((= $value \"4\")
    (start_list \"sel_str_z\")
    (mapcar 'add_list (list \"����������� ������� �����\" \"������� ��� �������\" \"������� ����� �����\"))
    (end_list)
    (mode_tile \"draw_rasst\" 0)
    (setq q_sel_v_pr 4)
    )
    ((= $value \"5\")
    (start_list \"sel_str_z\")
    (mapcar 'add_list (list \"����������� ������� ����� (�����)\" \"������� ����� ������\" \"������ ���� ������\" \"������� ��� �����\" \"������ �������� ��������\" \"������� ������ ����\"))
    (end_list)
    (mode_tile \"draw_rasst\" 0)
    (setq q_sel_v_pr 5)
    )
    )"
       )
     (action_tile "sel_str_z"; ����� ������ ������� �������
       "(cond
    ((= $value \"0\")
    (setq q_sel_str_z 0)
    )
   ((= $value \"1\")
    (setq q_sel_str_z 1)
    )
    ((= $value \"2\")
    (setq q_sel_str_z 2)
    )
    ((= $value \"3\")
    (setq q_sel_str_z 3)
    )
    ((= $value \"4\")
    (setq q_sel_str_z 4)
    )
    ((= $value \"5\")
    (setq q_sel_str_z 5)
    )
    )"
       )
     (action_tile "draw_line"
       "(if (= q_draw_line 0)
    (setq q_draw_line 1)
    (setq q_draw_line 0)
    )"
       )
     (action_tile "sel_uklon"; ����� ������ ������
       "(cond
    ((= $value \"0\")
    (setq q_sel_uklon 0)
    )
    ((= $value \"1\")
    (setq q_sel_uklon 1)
    )
    )"
       )
     (action_tile "draw_ugli"; �������� ���� ��������
       "(if (= q_draw_ugli 0)
    (setq q_draw_ugli 1)
    (setq q_draw_ugli 0)
    )"
       )
     (action_tile "draw_rasst"; �������� ���� ����������
       "(if (= q_draw_rasst 0)
    (setq q_draw_rasst 1)
    (setq q_draw_rasst 0)
    )"
       )
     (action_tile "optim_z"; �������������� �������
       "(if (= q_optim_z 0)
    (setq q_optim_z 1)
    (setq q_optim_z 0)
    )"
       )
     (if (eq (start_dialog) 0) (progn (unload_dialog dcl_id)(exit))(unload_dialog dcl_id))
     (if
       (not (or (eq (type mv) 'INT)(eq (type mv) 'REAL)))
       (setq mv (atof mv))
       )
     (if
       (not (or (eq (type mg) 'INT)(eq (type mg) 'REAL)))
       (setq mg (atof mg))
       )
     (if
       (not (or (eq (type usl_z) 'INT)(eq (type usl_z) 'REAL)))
       (setq usl_z (atof usl_z))
       )
     (file_dat_r_g_w "w" "mv" mv)
     (file_dat_r_g_w "w" "mg" mg)
     (file_dat_r_g_w "w" "usl_z" usl_z)
     (if
       (= q_xyz 1)
       (setq t1! (get_pt1!))
       (setq t1! (list (atof q_x) (atof q_y) (atof q_z) ))
       )
     (setq t0! (list (nth 0 (car(extract_coord_lwpolyline (entget pline)))) (nth 1 t1!)))
     (setq spisokv(extract_coord_lwpolyline (entget pline)))
     (setq n (length spisokv))
     (repeat n
       (setq sp_z(append sp_z (list(+(* (- (nth 1(car spisokv))(nth 1 t0!)) (/ (* mv (/ 1000 mg)) 1000))usl_z))))
       (setq spisokv (cdr spisokv))
       )
     (setq min_z (apply 'min sp_z))
     (setq max_z (1+(apply 'max sp_z)))

     ;                    ��������� ������
     (if (= q_ins_podval 1)
       (draw_profil_insert_block_podval q_sel_v_pr t0! mg mv min_z max_z usl_z 2Dpline)
       )
     ;                    ������ ����� �� �������
     (if (= q_draw_line 1)
       (draw_profil_vertical_line q_sel_v_pr t0! mg 2Dpline)
       )
     ;                    ����������� ���������� �� �������
     (if (= q_draw_rasst 1)
       (draw_profil_rasst 2Dpline t0! q_sel_v_pr mg)
       )
     ;                               ������� �������
     (if (= q_ins_z 1)
       (draw_profil_elevation q_chek_v_pr q_sel_v_pr t0! mg mv q_optim_z q_sel_str_z 2Dpline)
       )
     (princ q_optim_z)
     ;                         ������� ������/������
     (if (= q_draw_uklon 1)
       (draw_profil_downgrade q_chek_v_pr q_sel_v_pr t0! mg 2Dpline)
       )
     )
    )
  (princ)
  )
