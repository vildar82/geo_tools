(defun analisis_nomenclature_full_for_m500_sis_coor_sj ( t1 / ty tx stb1 y1 str1 x1 stb2 str2)
(setq ty (nth 0 t1)); ��������� ����� Y
(setq tx (nth 1 t1)); ��������� ����� x
  (if; �������� ��������� � �� �.������
    (and
     (and
       (or (and (< 6000 (nth 0 t1))(< (nth 0 t1) 10000))
   	   (= (nth 0 t1) 6000)
	   ); end or
       ); end and
     (and
       (or (and (< 4000 (nth 1 t1))(< (nth 1 t1) 6000))
	   (= (nth 1 t1) 4000)
	   ); end or
       ); end end
     ); end and � �������
  (progn ; ���������� ���� �����
  (if (or (and (< 6000 ty)(< ty 8000))(= 6000 ty))
     (progn(setq stb1 2)(setq y1 6000)))
  (if (or (and (< 8000 ty)(< ty 10000))(= 8000 ty))
     (progn(setq stb1 3)(setq y1 8000)))
    
  (if (or (and (< 4000 tx)(< tx 6000))(= 4000 tx))
     (progn(setq str1 10)(setq x1 4000)))
; ����� ���������� ����� � �������� ��� ������ ����� ������������ (12,13)
  (if (or (and (< y1 ty)(< ty (+ y1 1000)))(= y1 ty))
    (setq stb2 1))
  (if (or (and (< (+ y1 1000) ty)(< ty (+ y1 2000)))(= ty (+ y1 1000)))
    (setq stb2 2))
  
  (if (or (and (< (+ x1 1000) tx)(< tx (+ x1 2000)))(= tx (+ x1 1000)))
    (setq str2 0))
  (if (or (and (< x1 tx)(< tx (+ x1 1000)))(= x1 tx))
    (setq str2 2))
; ����� ���������� ����� � �������� ��� ������ ����� ������������ (�-�)
  (setq num1 (itoa (+ stb1 str1)))
    (setq num2 (cond
	       ((= (+ stb2 str2) 1) "�")
	       ((= (+ stb2 str2) 2) "�")
	       ((= (+ stb2 str2) 3) "�")
	       ((= (+ stb2 str2) 4) "�")
	       ); cond
	); setq
  (setq num3 (rtos (analisis_nomenclature_3ch_for_m500_sis_coor_oms t1) 2 0))
  (strcat num1 "-" num2 "-" num3)
   );progn T
  ); ����� if
  ); defun analisis_nomenclature_full_for_m500_sis_coor_sj
