(defun C:draw_table_offset_be_line ( / a b c t0 t1 t2 p p1 p2 p3 num line pline spisok sm dl alfa beta); ��������� ������ ����������� ��� ������ ������� ������������ �����
(geo_tools-help "draw_table_offset_be_line")
  (load_global_variable)
(setvar "CMDECHO" 0)
  (initget 1)(setq line (car (entsel "\n������ �������� �����: ")))
  (cond
    ((eq line nil)
     (alert "������ �� �������!"))
    ((not(eq (cdr(assoc 0 (entget line))) "LINE"))
     (alert "������� �� �����!"))
    ((eq (cdr(assoc 0 (entget line))) "LINE")
     (initget 1)(setq pline (car (entsel "\n������ ��������� ������: ")))
     (cond
       ((eq pline nil)
	(alert "������ �� �������!"))
       ((not(eq (cdr(assoc 0 (entget pline))) "LWPOLYLINE"))
	(alert "������� �� 2D ���������!"))
       ((eq (cdr(assoc 0 (entget pline))) "LWPOLYLINE")
	(setq 2Dpline_vla (vlax-ename->vla-object pline))
;;;	(setvar "NOMUTT" 1)
	(command "_.UNDO" "_begin")
	(command "_-purge" "_b" "������ ��� ������" "_n")
;;;	(setvar "NOMUTT" 0)
	(setq block (vla-Add (vla-get-Blocks active_document) (vlax-3D-point '(0 0 0)) "������ ��� ������"))
	(setq t0 '(0 0 0))
	(setq tl (mapcar '- t0 '(-48 16)))
	(setq t1 (cdr (assoc 10 (entget line))))
	(setq t2 (cdr (assoc 11 (entget line))))
	(entmake (list '(0 . "TEXT") (cons 10 t0) '(72 . 4) (cons 11 (polar t1 (angle t2 t1)4)) (cons 40 3) (cons 1 "�.1") (cons 7 (getvar "TEXTSTYLE"))))
	(entmake (list '(0 . "TEXT") (cons 10 t0) '(72 . 4) (cons 11 (polar t2 (angle t1 t2)4)) (cons 40 3) (cons 1 "�.2") (cons 7 (getvar "TEXTSTYLE"))))
	(entmake (list '(0 . "CIRCLE") (cons 10 t1) '(40 . 0.75)))
	(entmake (list '(0 . "CIRCLE") (cons 10 t2) '(40 . 0.75)))
	(setq spisok (extract_coord_lwpolyline (entget pline)))
	(setq num 1)
	(vla-put-Linetype (vla-AddLine block (vlax-3D-point t0)(vlax-3D-point (mapcar '+ t0 '(68 0)))) "Continuous")
	(vla-put-Linetype (vla-AddLine block (vlax-3D-point (mapcar '- t0 '(0 12)))(vlax-3D-point (mapcar '+ t0 '(48 -12)))) "Continuous")
	(vla-put-Linetype (vla-AddLine block (vlax-3D-point (mapcar '+ t0 '(48 -16)))(vlax-3D-point (mapcar '+ t0 '(68 -16)))) "Continuous")
	(vla-put-Linetype (vla-AddLine block (vlax-3D-point t0)(vlax-3D-point (mapcar '- t0 (list 0 (+ 12 (* 8 (1+(length spisok)))))))) "Continuous")
	(vla-put-Linetype (vla-AddLine block (vlax-3D-point (mapcar '+ t0 '(8 0)))(vlax-3D-point  (mapcar '- t0 (list  -8 (+ 12 (* 8 (length spisok))))))) "Continuous")
	(vla-put-Linetype (vla-AddLine block (vlax-3D-point (mapcar '+ t0 '(28 0)))(vlax-3D-point (mapcar '- t0 (list  -28 (+ 12 (* 8 (length spisok))))))) "Continuous")
	(vla-put-Linetype (vla-AddLine block (vlax-3D-point (mapcar '+ t0 '(48 0)))(vlax-3D-point (mapcar '- t0 (list  -48 (+ 12 (* 8 (1+(length spisok)))))))) "Continuous")
	(vla-put-Linetype (vla-AddLine block (vlax-3D-point (mapcar '+ t0 '(68 0)))(vlax-3D-point (mapcar '- t0 (list  -68 (+ 12 (* 8 (1+(length spisok)))))))) "Continuous")
	(vla-put-Linetype (vla-AddLine block (vlax-3D-point (mapcar '- t0 (list 0 (+ 12 (* 8 (1+(length spisok)))))))(vlax-3D-point (mapcar '- t0 (list  -68 (+ 12 (* 8 (1+(length spisok)))))))) "Continuous")
	(draw_text_vla block "�"(mapcar '+ t0 '(4 -3))2.8 4 T nil T T)
	(draw_text_vla block  "�/�"(mapcar '+ t0 '(4 -9))2.8 4 T nil T T)
	(draw_text_vla block  "�����"(mapcar '+ t0 '(18 -6))2.8 4 T nil T T)
	(draw_text_vla block  "��������"(mapcar '+ t0 '(38 -6))2.8 4 T nil T T)
	(draw_text_vla block  "�����"(mapcar '+ t0 '(58 -5))2.8 4 T nil T T)
	(draw_text_vla block  "�����"(mapcar '+ t0 '(58 -11))2.8 4 T nil T T)
	(draw_text_vla block  (strcat "����� ������ = " (rtos (distance t1 t2) 2 2)"�")(mapcar '- t0 (list -24 (+ 16 (* 8 (length spisok)))))2.8 4 T nil T T)
	(setq t0 (mapcar '- t0 '(0 12)))
;                          ������ �������                                
	(setq p1 (car spisok))
	(setq p2 (cadr spisok))
	(setq sm (*(distance t2 p1) (cos (- pi (- (- (angle t2 p1)(angle t1 t2)) (/ pi 2))))))
	(setq dl (*(distance t1 p1) (cos (- (angle t1 p1)(angle t1 t2)))))
	(draw_text_vla block (itoa num)(mapcar '+ t0 '(4 -4))2.4 4 T nil T T)
	(draw_text_vla block (rtos dl 2 2)(mapcar '+ t0 '(18 -4))2.4 4 T nil T T)
	(draw_text_vla block (rtos sm 2 2)(mapcar '+ t0 '(38 -4))2.4 4 T nil T T)
	(entmake (list '(0 . "TEXT") (cons 10 t0) '(72 . 4) (cons 11 (polar p1 (angle p2 p1)3.5)) (cons 40 3) (cons 1 (itoa num)) (cons 7 (getvar "TEXTSTYLE"))))
	(entmake (list '(0 . "CIRCLE") (cons 10 p1) '(40 . 0.75)))
	(setq t0 (mapcar '- t0 '(0 8)))
	(vla-put-Linetype (vla-AddLine block (vlax-3D-point t0)(vlax-3D-point (mapcar '+ t0 '(48 0)))) "Continuous")
	(setq num (1+ num))
;                          ������� ����������                                
	(repeat (- (length spisok) 2)
	  (setq p1 (car spisok))
	  (setq p2 (cadr spisok))
	  (setq p3 (caddr spisok))
	  (setq spisok (cdr spisok))
	  (setq sm (*(distance t2 p2) (cos (- pi (- (- (angle t2 p2)(angle t1 t2)) (/ pi 2)))))); ��������
	  (setq dl (*(distance t1 p2) (cos (- (angle t1 p2)(angle t1 t2))))); ������
	  (draw_text_vla block (itoa num)(mapcar '+ t0 '(4 -4))2.4 4 T nil T T)
	  (draw_text_vla block (rtos dl 2 2)(mapcar '+ t0 '(18 -4))2.4 4 T nil T T)
	  (draw_text_vla block (rtos sm 2 2)(mapcar '+ t0 '(38 -4))2.4 4 T nil T T)
	  (cond
	    ((> (calculation_angle_be_3points p1 p2 p3) (calculation_angle_be_3points p3 p2 p1))
	     (setq p (polar p2 (+(angle p2 p3) (/(calculation_angle_be_3points p1 p2 p3)2)) 3.5))
	     )
	    ((< (calculation_angle_be_3points p1 p2 p3) (calculation_angle_be_3points p3 p2 p1))
	     (setq p (polar p2 (+(angle p2 p1) (/(calculation_angle_be_3points p3 p2 p1)2)) 3.5))
	     )
	    ((= (calculation_angle_be_3points p1 p2 p3) (calculation_angle_be_3points p3 p2 p1))
	     (setq p (polar p2 (+(angle p2 p3) (/ pi 2)) 3.5))
	     )
	    )
	  (entmake (list '(0 . "TEXT") (cons 10 t0) '(72 . 4) (cons 11 p) (cons 40 3) (cons 1 (itoa num)) (cons 7 (getvar "TEXTSTYLE"))))
	  (entmake (list '(0 . "CIRCLE") (cons 10 p2) '(40 . 0.75)))
	  (setq t0 (mapcar '- t0 '(0 8)))
	  (vla-put-Linetype (vla-AddLine block (vlax-3D-point t0)(vlax-3D-point (mapcar '+ t0 '(48 0)))) "Continuous")
	  (setq num (1+ num))
	  )
;                          ��������� �������                                
	(setq p1 (car spisok))
	(setq p2 (cadr spisok))
	(setq sm (*(distance t2 p2) (cos (- pi (- (- (angle t2 p2)(angle t1 t2)) (/ pi 2))))))
	(setq dl (*(distance t1 p2) (cos (- (angle t1 p2)(angle t1 t2)))))
	(draw_text_vla block (itoa num)(mapcar '+ t0 '(4 -4))2.4 4 T nil T T)
	(draw_text_vla block (rtos dl 2 2)(mapcar '+ t0 '(18 -4))2.4 4 T nil T T)
	(draw_text_vla block (rtos sm 2 2)(mapcar '+ t0 '(38 -4))2.4 4 T nil T T)
	(entmake (list '(0 . "TEXT") (cons 10 t0) '(72 . 4) (cons 11 (polar p2 (angle p1 p2)3.5)) (cons 40 3) (cons 1 (itoa num)) (cons 7 (getvar "TEXTSTYLE"))))
	(entmake (list '(0 . "CIRCLE") (cons 10 p2) '(40 . 0.75)))
	(setq t0 (mapcar '- t0 '(0 8)))
	(vla-put-Linetype (vla-AddLine block (vlax-3D-point t0)(vlax-3D-point (mapcar '+ t0 '(48 0)))) "Continuous")
	(setq spisok (extract_coord_lwpolyline (entget pline)))
	(setq Ed 0)
	(repeat (1- (length spisok))
	  (setq p1 (car spisok))
	  (setq spisok (cdr spisok))
	  (setq p2 (car spisok))
	  (draw_text_vla block (rtos (distance p1 p2) 2 2)(mapcar '+ tl '(10 -4))2.4 4 T nil T T)
	  (setq tl (mapcar '- tl '(0 8)))
	  (vla-put-Linetype (vla-AddLine block (vlax-3D-point tl)(vlax-3D-point (mapcar '+ tl '(20 0)))) "Continuous")
	  (setq Ed (+ Ed (distance p1 p2)))
	  )
	(draw_text_vla block (strcat "\U+03A3L="(rtos Ed 2 2) "�")(mapcar '+ tl '(10 -6))2.4 4 T nil T T)
	(princ "\n����� ����� ������� ������ ������: ")
	(command "_insert" "������ ��� ������" pause "1" "1" 0.0)
	(command "_explode" (entlast))
	(setvar "NOMUTT" 1)
	(terpri)
	(command "_-purge" "_b" "������ ��� ������" "_n")
	(setvar "NOMUTT" 0)
	(command "_.UNDO" "_end")
	(setvar "CMDECHO" 1)
	)
       )
     )
    )
  (princ)
  )
