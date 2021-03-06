(defun draw_blok_shtamp (shtamp auto_insert / patch_blank patch list_cell block list_line_points big_list list_b_line_points)
  ; ������ ����� �� �������� ������ ������
  ; 
  (load_global_variable)
  (setq patch_blank (strcat *geo_tools_user_dat_dir* "\\template\\blank.sht"))
;;;  (setq patch (file_dat_r_g_w "r" "patch_sh" nil))
  (setq patch (PL:GetD "patch_current_shtamp"))
  (if (eq patch nil) (setq patch patch_blank))
  (if (not (file_check patch))
    (setq patch patch_blank))
  (setq list_cell(file_read_to_list patch))
  (if(tblobjname "block" "�����")(command "_-purge" "�" "�����" "�"))
  (setq block (vla-Add (vla-get-Blocks active_document) (vlax-3D-point '(0 0 0)) "�����"))
  (cond
    ((eq shtamp "���������")
     (setq big_list
	    (list
	      (list (nth (1- 50) list_cell) 2 10 '(-5.0 12.5 0.0)5)
	      (list (vl-string-left-trim "#"(nth (1- 51) list_cell)) 2 10 '(-5.0 5.0 0.0)5)
	      (list (strcat (nth (1- 5) list_cell)
			    (if (/= (nth (1- 5) list_cell) "")"\\P""")
			    (nth (1- 6) list_cell)
			    (if (or(/= (nth (1- 6) list_cell) "")(/= (nth (1- 7) list_cell) ""))"\\P""")
			    (nth (1- 7) list_cell)
			    (if (/= (nth (1- 8) list_cell) "")"\\P{\\L""")
			    (nth (1- 8) list_cell)
			    (if (/= (nth (1- 8) list_cell) "")"}""")
			    ) 1.7 65 '(-42.5 7.5 0.0)5)
	      (list (nth (1- 34) list_cell) 2 16 '(-122.00 12.5 0.0)5)
	      (list (strcat(if (/= (nth (1- 35) list_cell) "")"\\pxl1,ql;""")(nth (1- 35) list_cell)) 2 15 '(-82.50 12.5 0.0)5)
	      (list (strcat(if (/= (nth (1- 36) list_cell) "")"\\pxl1,ql;""")(nth (1- 36) list_cell)) 2 24 '(-102.00 12.5 0.0)5)
	      (list (strcat(if (/= (nth (1- 37) list_cell) "")"\\pxl1,ql;""")(nth (1- 37) list_cell)) 2 16 '(-122.00 7.5 0.0)5)
	      (list (strcat(if (/= (nth (1- 38) list_cell) "")"\\pxl1,ql;""")(nth (1- 38) list_cell)) 2 15 '(-82.50 7.5 0.0)5)
	      (list (strcat(if (/= (nth (1- 39) list_cell) "")"\\pxl1,ql;""")(nth (1- 39) list_cell)) 2 24 '(-102.00 7.5 0.0)5)
	      (list (strcat(if (/= (nth (1- 40) list_cell) "")"\\pxl1,ql;""")(nth (1- 40) list_cell)) 2 16 '(-122.00 2.5 0.0)5)
	      (list (strcat(if (/= (nth (1- 41) list_cell) "")"\\pxl1,ql;""")(nth (1- 41) list_cell)) 2 15 '(-82.50 2.5 0.0)5)
	      (list (strcat(if (/= (nth (1- 42) list_cell) "")"\\pxl1,ql;""")(nth (1- 42) list_cell)) 2 24 '(-102.00 2.5 0.0)5)
	      ))
     (setq list_line_points
	    (list
	      (list'(0.000 15.000 0.000) '(0.000 0.000 0.000))
	      (list'(-130.000 15.000 0.000) '(-130.000 0.000 0.000))
	      (list'(-10.000 10.000 0.000) '(0.000 10.000 0.000))
	      (list'(-130.000 5.000 0.000) '(-75.000 5.000 0.000))
	      (list'(-130.000 10.000 0.000) '(-75.000 10.000 0.000))
	      (list'(-114.000 15.000 0.000) '(-114.000 0.000 0.000))
	      (list'(-90.000 15.000 0.000) '(-90.000 0.000 0.000))
	      (list'(-75.000 15.000 0.000) '(-75.000 0.000 0.000))
	      (list'(-10.000 15.000 0.000) '(-10.000 0.000 0.000))
	      (list'(0.000 15.000 0.000) '(-130.000 15.000 0.000))
	      (list'(0.000 0.000 0.000) '(-130.000 0.000 0.000))
	      ))
     )
    ((eq shtamp "�������")
     (setq big_list
	    (list
	      (list (strcat (nth (1- 1) list_cell)
			    (if (or(/= (nth (1- 1) list_cell) "")(/= (nth (1- 2) list_cell) ""))"\\P""")
			    (nth (1- 2) list_cell)) 5 80 '(-90.0 122.5 0.0)5)
	      (list (strcat (nth (1- 3) list_cell)
			    (if (or(/= (nth (1- 3) list_cell) "")(/= (nth (1- 4) list_cell) ""))"\\P""")
			    (nth (1- 4) list_cell)) 4 50 '(-25.0 122.5 0.0)5)
	      (list (strcat (nth (1- 5) list_cell)
			    (if (/= (nth (1- 5) list_cell) "")"\\P""")
			    (nth (1- 6) list_cell)
			    (if (or(/= (nth (1- 6) list_cell) "")(/= (nth (1- 7) list_cell) ""))"\\P""")
			    (nth (1- 7) list_cell)
			    (if (/= (nth (1- 8) list_cell) "")"\\P\\P{\\L""")
			    (nth (1- 8) list_cell)
			    (if (/= (nth (1- 8) list_cell) "")"}""")
			    ) 3 130 '(-65.0 97.5 0.0)5)
	      (list (strcat (nth (1- 9) list_cell)
			    (if (or(/= (nth (1- 9) list_cell) "")(/= (nth (1- 10) list_cell) ""))"\\P""")
			    (nth (1- 10) list_cell)) 3 40 '(-110.0 73.75 0.0)5)
	      (list (strcat (nth (1- 11) list_cell)
			    (if (or(/= (nth (1- 11) list_cell) "")(/= (nth (1- 12) list_cell) ""))"\\P""")
			    (nth (1- 12) list_cell)) 2.5 40 '(-70.0 73.75 0.0)5)
	      (list (strcat (nth (1- 13) list_cell)
			    (if (or(/= (nth (1- 13) list_cell) "")(/= (nth (1- 14) list_cell) ""))"\\P""")
			    (nth (1- 14) list_cell)) 3 50 '(-25.0 73.75 0.0)5)
	      (list (strcat (nth (1- 15) list_cell)
			    (if (or(/= (nth (1- 15) list_cell) "")(/= (nth (1- 16) list_cell) ""))"\\P""")
			    (nth (1- 16) list_cell)) 3 80 '(-90.0 61.25 0.0)5)
	      (list (strcat (nth (1- 17) list_cell)
			    (if (or(/= (nth (1- 17) list_cell) "")(/= (nth (1- 18) list_cell) ""))"\\P""")
			    (nth (1- 18) list_cell)) 3 50 '(-25.0 61.25 0.0)5)
	      (list (strcat (nth (1- 19) list_cell)
			    (if (or(/= (nth (1- 19) list_cell) "")(/= (nth (1- 20) list_cell) ""))"\\P""")
			    (nth (1- 20) list_cell)) 3 40 '(-110.0 48.75 0.0)5)
	      (list (strcat (nth (1- 21) list_cell)
			    (if (or(/= (nth (1- 21) list_cell) "")(/= (nth (1- 22) list_cell) ""))"\\P""")
			    (nth (1- 22) list_cell)) 3 40 '(-70.0 48.75 0.0)5)
	      (list (strcat (nth (1- 23) list_cell)
			    (if (/= (nth (1- 23) list_cell) "")"\\P""")
			    (nth (1- 24) list_cell)
			    (if (or(/= (nth (1- 24) list_cell) "")(/= (nth (1- 25) list_cell) ""))"\\P""")
			    (nth (1- 25) list_cell)
			    (if (/= (nth (1- 26) list_cell) "")"\\P""")
			    (nth (1- 26) list_cell)) 3 50 '(-25.0 42.50 0.0)5)
	      (list (strcat (nth (1- 27) list_cell)
			    (if (or(/= (nth (1- 27) list_cell) "")(/= (nth (1- 28) list_cell) ""))"\\P""")
			    (nth (1- 28) list_cell)) 3 40 '(-110.0 36.25 0.0)5)
	      (list (strcat (nth (1- 29) list_cell)
			    (if (or(/= (nth (1- 29) list_cell) "")(/= (nth (1- 30) list_cell) ""))"\\P""")
			    (nth (1- 30) list_cell)) 3 40 '(-70.0 36.25 0.0)5)
	      (list (strcat(if (/= (nth (1- 31) list_cell) "")"\\pxl1,ql;""")(nth (1- 31) list_cell)) 3 40 '(-110.0 26.25 0.0)5)
	      (list (strcat(if (/= (nth (1- 34) list_cell) "")"\\pxl1,ql;""")(nth (1- 34) list_cell)) 3 40 '(-110.0 18.75 0.0)5)
	      (list (strcat(if (/= (nth (1- 37) list_cell) "")"\\pxl1,ql;""")(nth (1- 37) list_cell)) 3 40 '(-110.0 11.25 0.0)5)
	      (list (strcat(if (/= (nth (1- 40) list_cell) "")"\\pxl1,ql;""")(nth (1- 40) list_cell)) 3 40 '(-110.0 3.75 0.0)5)
	      (list (strcat(if (/= (nth (1- 32) list_cell) "")"\\pxl1,ql;""")(nth (1- 32) list_cell)) 3 40 '(-70.0 26.25 0.0)5)
	      (list (strcat(if (/= (nth (1- 35) list_cell) "")"\\pxl1,ql;""")(nth (1- 35) list_cell)) 3 40 '(-70.0 18.75 0.0)5)
	      (list (strcat(if (/= (nth (1- 38) list_cell) "")"\\pxl1,ql;""")(nth (1- 38) list_cell)) 3 40 '(-70.0 11.25 0.0)5)
	      (list (strcat(if (/= (nth (1- 41) list_cell) "")"\\pxl1,ql;""")(nth (1- 41) list_cell)) 3 40 '(-70.0 3.75 0.0)5)
	      (list (strcat(if (/= (nth (1- 33) list_cell) "")"\\pxl1,ql;""")(nth (1- 33) list_cell)) 3 50 '(-25.0 26.25 0.0)5)
	      (list (strcat(if (/= (nth (1- 36) list_cell) "")"\\pxl1,ql;""")(nth (1- 36) list_cell)) 3 50 '(-25.0 18.75 0.0)5)
	      (list (strcat(if (/= (nth (1- 39) list_cell) "")"\\pxl1,ql;""")(nth (1- 39) list_cell)) 3 50 '(-25.0 11.25 0.0)5)
	      (list (strcat(if (/= (nth (1- 42) list_cell) "")"\\pxl1,ql;""")(nth (1- 42) list_cell)) 3 50 '(-25.0 3.75 0.0)5)
	      ))
     (setq list_line_points
	    (list
	      (list'(0.000 0.000 0.000) '(-130.000 0.000 0.000))
	      (list'(0.000 130.000 0.000) '(0.000 0.000 0.000))
	      (list'(-50.000 115.000 0.000) '(-50.000 130.000 0.000))
	      (list'(-50.000 80.000 0.000) '(-50.000 0.000 0.000))
	      (list'(-90.000 55.000 0.000) '(-90.000 0.000 0.000))
	      (list'(-90.000 80.000 0.000) '(-90.000 67.500 0.000))
	      (list'(-130.000 0.000 0.000) '(-130.000 130.000 0.000))
	      (list'(-50.000 42.500 0.000) '(-130.000 42.500 0.000))
	      (list'(0.000 55.000 0.000) '(-130.000 55.000 0.000))
	      (list'(0.000 67.500 0.000) '(-130.000 67.500 0.000))
	      (list'(0.000 80.000 0.000) '(-130.000 80.000 0.000))
	      (list'(0.000 7.500 0.000) '(-130.000 7.500 0.000))
	      (list'(0.000 15.000 0.000) '(-130.000 15.000 0.000))
	      (list'(0.000 22.500 0.000) '(-130.000 22.500 0.000))
	      (list'(0.000 30.000 0.000) '(-130.000 30.000 0.000))
	      (list'(0.000 115.000 0.000) '(-130.000 115.000 0.000))
	      (list'(0.000 130.000 0.000) '(-130.000 130.000 0.000))
	      ))
     )
    ((eq shtamp "����")
     (setq big_list
	    (list
	      (list (nth (1- 77) list_cell)2 10 '(-125.000 52.500 0.000)5)
	      (list (nth (1- 78) list_cell)2 10 '(-125.000 47.500 0.000)5)
	      (list (nth (1- 79) list_cell)2 10 '(-125.000 42.500 0.000)5)
	      (list (nth (1- 80) list_cell)2 10 '(-125.000 37.500 0.000)5)
	      (list (nth (1- 72) list_cell)2 13 '(-136.250 52.500 0.000)5)
	      (list (nth (1- 73) list_cell)2 13 '(-136.250 47.500 0.000)5)
	      (list (nth (1- 74) list_cell)2 13 '(-136.250 42.500 0.000)5)
	      (list (nth (1- 75) list_cell)2 13 '(-136.250 37.500 0.000)5)
	      (list (nth (1- 67) list_cell)2 13 '(-148.750 52.500 0.000)5)
	      (list (nth (1- 68) list_cell)2 13 '(-148.750 47.500 0.000)5)
	      (list (nth (1- 69) list_cell)2 13 '(-148.750 42.500 0.000)5)
	      (list (nth (1- 70) list_cell)2 13 '(-148.750 37.500 0.000)5)
	      (list (nth (1- 62) list_cell)2 10 '(-160.000 52.500 0.000)5)
	      (list (nth (1- 81) list_cell)2 10 '(-125.000 32.500 0.000)5)
	      (list (nth (1- 76) list_cell)2 13 '(-136.250 32.500 0.000)5)
	      (list (nth (1- 71) list_cell)2 13 '(-148.750 32.500 0.000)5)
	      (list (nth (1- 63) list_cell)2 10 '(-160.000 47.500 0.000)5)
	      (list (nth (1- 64) list_cell)2 10 '(-160.000 42.500 0.000)5)
	      (list (nth (1- 65) list_cell)2 10 '(-160.000 37.500 0.000)5)
	      (list (nth (1- 66) list_cell)2 10 '(-160.000 32.500 0.000)5)
	      (list (nth (1- 61) list_cell)2 10 '(-170.000 32.500 0.000)5)
	      (list (nth (1- 60) list_cell)2 10 '(-170.000 37.500 0.000)5)
	      (list (nth (1- 59) list_cell)2 10 '(-170.000 42.500 0.000)5)
	      (list (nth (1- 58) list_cell)2 10 '(-170.000 47.500 0.000)5)
	      (list (nth (1- 57) list_cell)2 10 '(-170.000 52.500 0.000)5)
	      (list (nth (1- 52) list_cell)2 10 '(-180.000 52.500 0.000)5)
	      (list (nth (1- 53) list_cell)2 10 '(-180.000 47.500 0.000)5)
	      (list (nth (1- 54) list_cell)2 10 '(-180.000 42.500 0.000)5)
	      (list (nth (1- 55) list_cell)2 10 '(-180.000 37.500 0.000)5)
	      (list (strcat(if (/= (nth (1- 82) list_cell) "")"\\pxl1,ql;""")(nth (1- 82) list_cell))2 10 '(-125.000 27.500 0.000)5)
	      (list (strcat(if (/= (nth (1- 83) list_cell) "")"\\pxl1,ql;""")(nth (1- 83) list_cell))2 10 '(-125.000 22.500 0.000)5)
	      (list (strcat(if (/= (nth (1- 84) list_cell) "")"\\pxl1,ql;""")(nth (1- 84) list_cell))2 10 '(-125.000 17.500 0.000)5)
	      (list (strcat(if (/= (nth (1- 94) list_cell) "")"\\pxl1,ql;""")(nth (1- 94) list_cell))2 10 '(-125.000 12.500 0.000)5)
	      (list (strcat(if (/= (nth (1- 95) list_cell) "")"\\pxl1,ql;""")(nth (1- 95) list_cell))2 10 '(-125.000 7.500 0.000)5)
	      (list (strcat(if (/= (nth (1- 96) list_cell) "")"\\pxl1,ql;""")(nth (1- 96) list_cell))2 10 '(-125.000 2.500 0.000)5)
	      (list (strcat(if (/= (nth (1- 93) list_cell) "")"\\pxl1,ql;""")(nth (1- 93) list_cell))2 13 '(-136.250 2.500 0.000)5)
	      (list (strcat(if (/= (nth (1- 92) list_cell) "")"\\pxl1,ql;""")(nth (1- 92) list_cell))2 13 '(-136.250 7.500 0.000)5)
	      (list (strcat(if (/= (nth (1- 91) list_cell) "")"\\pxl1,ql;""")(nth (1- 91) list_cell))2 13 '(-136.250 12.500 0.000)5)
	      (list (strcat(if (/= (nth (1- 41) list_cell) "")"\\pxl1,ql;""")(nth (1- 41) list_cell))2 13 '(-136.250 17.500 0.000)5)
	      (list (strcat(if (/= (nth (1- 38) list_cell) "")"\\pxl1,ql;""")(nth (1- 38) list_cell))2 13 '(-136.250 22.500 0.000)5)
	      (list (strcat(if (/= (nth (1- 35) list_cell) "")"\\pxl1,ql;""")(nth (1- 35) list_cell))2 13 '(-136.250 27.500 0.000)5)
	      (list (strcat(if (/= (nth (1- 36) list_cell) "")"\\pxl1,ql;""")(nth (1- 36) list_cell))2 23 '(-153.750 27.500 0.000)5)
	      (list (strcat(if (/= (nth (1- 39) list_cell) "")"\\pxl1,ql;""")(nth (1- 39) list_cell))2 23 '(-153.750 22.500 0.000)5)
	      (list (strcat(if (/= (nth (1- 42) list_cell) "")"\\pxl1,ql;""")(nth (1- 42) list_cell))2 23 '(-153.750 17.500 0.000)5)
	      (list (strcat(if (/= (nth (1- 88) list_cell) "")"\\pxl1,ql;""")(nth (1- 88) list_cell))2 23 '(-153.750 12.500 0.000)5)
	      (list (strcat(if (/= (nth (1- 89) list_cell) "")"\\pxl1,ql;""")(nth (1- 89) list_cell))2 23 '(-153.750 7.500 0.000)5)
	      (list (strcat(if (/= (nth (1- 90) list_cell) "")"\\pxl1,ql;""")(nth (1- 90) list_cell))2 23 '(-153.750 2.500 0.000)5)
	      (list (strcat(if (/= (nth (1- 34) list_cell) "")"\\pxl1,ql;""")(nth (1- 34) list_cell))2 20 '(-175.000 27.500 0.000)5)
	      (list (strcat(if (/= (nth (1- 37) list_cell) "")"\\pxl1,ql;""")(nth (1- 37) list_cell))2 20 '(-175.000 22.500 0.000)5)
	      (list (strcat(if (/= (nth (1- 40) list_cell) "")"\\pxl1,ql;""")(nth (1- 40) list_cell))2 20 '(-175.000 17.500 0.000)5)
	      (list (strcat(if (/= (nth (1- 85) list_cell) "")"\\pxl1,ql;""")(nth (1- 85) list_cell))2 20 '(-175.000 12.500 0.000)5)
	      (list (strcat(if (/= (nth (1- 86) list_cell) "")"\\pxl1,ql;""")(nth (1- 86) list_cell))2 20 '(-175.000 7.500 0.000)5)
	      (list (strcat(if (/= (nth (1- 87) list_cell) "")"\\pxl1,ql;""")(nth (1- 87) list_cell))2 20 '(-175.000 2.500 0.000)5)
	      (list (strcat (nth (1- 14) list_cell)
			    (if (or(/= (nth (1- 14) list_cell) "")(/= (nth (1- 97) list_cell) ""))"\\P""")
			    (nth (1- 97) list_cell))3 120 '(-60.000 50.000 0.000)5)
	      (list (strcat (nth (1- 98) list_cell)
			    (if (/= (nth (1- 98) list_cell) "")"\\P""")
			    (nth (1- 8) list_cell)
			    (if (or(/= (nth (1- 8) list_cell) "")(/= (nth (1- 99) list_cell) ""))"\\P""")
			    (nth (1- 99) list_cell))3 70 '(-85.000 22.500 0.000)5)
	      (list (strcat (nth (1- 107) list_cell)
			    (if (/= (nth (1- 107) list_cell) "")"\\P""")
			    (nth (1- 108) list_cell)
			    (if (or(/= (nth (1- 108) list_cell) "")(/= (nth (1- 109) list_cell) ""))"\\P""")
			    (nth (1- 109) list_cell))3 70 '(-85.000 7.500 0.000)5)
	      (list (strcat (nth (1- 110) list_cell)
			    (if (/= (nth (1- 110) list_cell) "")"\\P""")
			    (nth (1- 1) list_cell)
			    (if (or(/= (nth (1- 1) list_cell) "")(/= (nth (1- 111) list_cell) ""))"\\P""")
			    (nth (1- 111) list_cell))3 50 '(-25.000 7.500 0.000)5)
	      (list (strcat (nth (1- 101) list_cell)
			    (if (or(/= (nth (1- 101) list_cell) "")(/= (nth (1- 102) list_cell) ""))"\\P""")
			    (nth (1- 102) list_cell))3 15 '(-42.500 20.000 0.000)5)
	      (list (strcat (nth (1- 104) list_cell)
			    (if (or(/= (nth (1- 104) list_cell) "")(/= (nth (1- 105) list_cell) ""))"\\P""")
			    (nth (1- 105) list_cell))3 20 '(-10.000 20.000 0.000)5)
	      (list (strcat (vl-string-left-trim "#"(nth (1- 51) list_cell))
			    (if (or(/= (nth (1- 51) list_cell) "")(/= (nth (1- 106) list_cell) ""))"\\P""")
			    (nth (1- 106) list_cell))3 15 '(-27.500 20.000 0.000)5)
	      (list (nth (1- 103) list_cell)2.5 20 '(-10.000 27.500 0.000)5)
	      (list (nth (1- 50) list_cell)2.5 15 '(-27.500 27.500 0.000)5)
	      (list (nth (1- 56) list_cell)2.0 10 '(-180.000 32.500 0.000)5)
	      (list (nth (1- 100) list_cell)2.5 15 '(-42.500 27.500 0.000)5)
	      (list (strcat (nth (1- 5) list_cell)
			    (if (/= (nth (1- 5) list_cell) "")"\\P""")
			    (nth (1- 6) list_cell)
			    (if (or(/= (nth (1- 6) list_cell) "")(/= (nth (1- 7) list_cell) ""))"\\P""")
			    (nth (1- 7) list_cell))2.8 120 '(-60.000 37.500 0.000)5)
	      ))
     (setq list_b_line_points
	    (list
	      (list'(-185.000 35.000 0.000) '(-120.000 35.000 0.000))
	      (list'(-120.000 45.000 0.000) '(0.000 45.000 0.000))
	      (list'(-185.000 55.000 0.000) '(0.000 55.000 0.000))
	      (list'(-185.000 30.000 0.000) '(0.000 30.000 0.000))
	      (list'(-120.000 15.000 0.000) '(0.000 15.000 0.000))
	      (list'(0.000 0.000 0.000) '(-185.000 0.000 0.000))
	      (list'(-175.000 55.000 0.000) '(-175.000 30.000 0.000))
	      (list'(-185.000 0.000 0.000) '(-185.000 55.000 0.000))
	      (list'(-142.500 0.000 0.000) '(-142.500 55.000 0.000))
	      (list'(-155.000 55.000 0.000) '(-155.000 30.000 0.000))
	      (list'(-165.000 0.000 0.000) '(-165.000 55.000 0.000))
	      (list'(-130.000 0.000 0.000) '(-130.000 55.000 0.000))
	      (list'(-120.000 0.000 0.000) '(-120.000 55.000 0.000))
	      (list'(-50.000 25.000 0.000) '(0.000 25.000 0.000))
	      (list'(-35.000 30.000 0.000) '(-35.000 15.000 0.000))
	      (list'(-50.000 30.000 0.000) '(-50.000 0.000 0.000))
	      (list'(-20.000 30.000 0.000) '(-20.000 15.000 0.000))
	      (list'(0.000 55.000 0.000) '(0.000 0.000 0.000))
	      ))
          (setq list_line_points
	    (list
	      (list'(-185.000 5.000 0.000) '(-120.000 5.000 0.000))
	      (list'(-185.000 10.000 0.000) '(-120.000 10.000 0.000))
	      (list'(-185.000 15.000 0.000) '(-120.000 15.000 0.000))
	      (list'(-185.000 25.000 0.000) '(-120.000 25.000 0.000))
	      (list'(-185.000 20.000 0.000) '(-120.000 20.000 0.000))
	      (list'(-185.000 40.000 0.000) '(-120.000 40.000 0.000))
	      (list'(-185.000 45.000 0.000) '(-120.000 45.000 0.000))
	      (list'(-185.000 50.000 0.000) '(-120.000 50.000 0.000))
	      )
		)
     )
    )
  (mapcar '(lambda (x) (if (/= (nth 0 x) "") (draw_mtext_vla block (nth 0 x)(nth 1 x)(nth 2 x)(nth 3 x)(nth 4 x))))
	big_list)
  (mapcar '(lambda (x)(vla-put-Linetype (vla-AddLine block (vlax-3D-point(nth 0 x))(vlax-3D-point(nth 1 x))) "Continuous"))
	list_line_points)
  (if list_b_line_points
    (mapcar '(lambda (x)
	       (draw-line-vla-by-list-property
		 (list
		   (cons "spece" block)
		   (cons "StartPoint" (nth 0 x))
		   (cons "EndPoint" (nth 1 x))
		   (cons "Linetype" "Continuous")
		   (cons "Lineweight" 30)
		   ))
;;;	       (vla-put-Linetype (vla-AddLine block (vlax-3D-point(nth 0 x))(vlax-3D-point(nth 1 x))) "Continuous")
	       )
	list_b_line_points)
    )
  (if (and(= (ascii(nth (1- 51) list_cell))35)(or (eq shtamp "����")(eq shtamp "���������")))
    (file_write_list_to_file patch(edit_replase_in_list_eliment_be_number (strcat"#"(rtos(1+(atof(vl-string-left-trim "#"(nth (1- 51) list_cell))))2 0))51 list_cell))
    )
  (if (eq auto_insert "���")
    (progn
      (princ "\n����� ����� ������� ������: ")
      (command "_insert" "�����" "_S" (/ 1 (getvar "CANNOSCALEVALUE")) pause 0.0)
      (command "_explode" (entlast))
      (if(tblobjname "block" "�����")(command "_-purge" "�" "�����" "�"))
      )
    )
  (princ)
  )
