(defun profile-calculation-and-draw-length-and-downgrade-by-pline (block-profile-obj 2Dpline list-select-string /
								   ang curent-text-style down-y-coor-in-curent-string h1 h2 horizontal-scale
								   hs ht mg mv n number-string n_00 obj-list p1 p2 pro pro! profile-parameters
								   profile-string-list s scale-coefficient spisokv styl t1 t1-
								   t11 t1_draw_uklon t2 t2- t22 t2_draw_uklon t3 up-y-coor-in-curent-string vertical-scale
								   dec)
    ;чертит расстояния/уклоны на профиле
  (if(=(vlax-ldata-get "geo_tools_dictionary" "profile-mode")1)
    (progn
      (setq up-y-coor-in-curent-string (nth 1 list-select-string))
      (setq down-y-coor-in-curent-string(nth 2 list-select-string))
      (setq number-string(nth 0 list-select-string))
      (setq profile-string-list(vlax-ldata-get block-profile-obj"profile-string-list"))
      (setq list-select-string(nth number-string profile-string-list))
      (setq horizontal-scale (vlax-ldata-get block-profile-obj"horizontal-scale"))
      (setq vertical-scale (vlax-ldata-get block-profile-obj"vertical-scale"))
      (setq profile-parameters(vlax-ldata-get block-profile-obj"profile-parameters"))
      (setq scale-coefficient (/ 1.0 (/ 1000.0 horizontal-scale)))
      (setq curent-text-style (nth 1 profile-parameters))
      (if(or(eq curent-text-style "***Текущий***")(not(member curent-text-style(get-text-style-list))))
	(setq curent-text-style(getvar "TEXTSTYLE")))
      (setq ht (*(atof(nth 6 list-select-string))scale-coefficient))
      (setq hs (*(atof(nth 2 list-select-string))scale-coefficient))
      (setq pro! (if (eq (nth 3 list-select-string)"0") 1000.0 1.0))
      (setq n_00(atoi(nth 5 list-select-string)))
      (setq dec (atoi(nth 4 list-select-string)))
      (setq flag T)
      )
    (progn
      (if(setq down-y-coor-in-curent-string(cadr(getpoint"\nУкажи точку на нижней линии строки: ")))
	(if(setq up-y-coor-in-curent-string(cadr(getpoint "\nУкажи точку на верхней линии строки: ")))
	  (progn
	    (setq horizontal-scale (cadr(assoc "horizontal-scale" (vlax-ldata-get "geo_tools_dictionary" "old-profile-item-list"))))
	    (setq vertical-scale (cadr(assoc "vertical-scale" (vlax-ldata-get "geo_tools_dictionary" "old-profile-item-list"))))
	    (setq scale-coefficient (/ 1.0 (/ 1000.0 horizontal-scale)))
	    (setq curent-text-style(getvar "TEXTSTYLE"))
	    (setq ht (*(cadr(assoc "height-text" (vlax-ldata-get "geo_tools_dictionary" "old-profile-item-list")))
		       scale-coefficient))
	    (setq hs (abs(- up-y-coor-in-curent-string down-y-coor-in-curent-string)))
	    (setq dec (cadr(assoc "accuracy" (vlax-ldata-get "geo_tools_dictionary" "old-profile-item-list"))))
	    (setq pro! (if (eq (cadr(assoc "sel-uklon" (vlax-ldata-get "geo_tools_dictionary" "old-profile-item-list")))0) 1000.0 1.0))
	    (setq n_00(cadr(assoc "accuracy-downgrade" (vlax-ldata-get "geo_tools_dictionary" "old-profile-item-list"))))
	    (setq flag T)
	    )))
      )
    )
  (if flag
    (progn
      (setq mv vertical-scale
	    mg horizontal-scale
	    t1_draw_uklon (list 0 down-y-coor-in-curent-string)
	    t2_draw_uklon (list 0 up-y-coor-in-curent-string)
	    styl curent-text-style)
      (if (=(type 2Dpline) 'list)
	(setq spisokv 2Dpline)
	(setq spisokv(extract_coord_lwpolyline (entget 2Dpline))))
      (if(and
	   (>(nth 1(nth 0 spisokv))down-y-coor-in-curent-string)
	   (< (nth 1(nth 0 spisokv))up-y-coor-in-curent-string)
	   (=(length spisokv)2))
	(princ(setq spisokv(list (list(nth 0(car spisokv))(/(get-elevation-be-text-obj (car(entsel "\nВыбери текст с первой отметкой: ")))(/ (* vertical-scale (/ 1000.0 horizontal-scale)) 1000.0))0)
			     (list(nth 0(nth 1 spisokv))(/(get-elevation-be-text-obj (car(entsel "\nВыбери текст со второй отметкой: ")))(/ (* vertical-scale (/ 1000.0 horizontal-scale)) 1000.0))0))))
	)
      
      (setq n (1- (length spisokv)))
      (setq obj-list(append obj-list (list (draw-line-vla model_spece (list(nth 0 (car spisokv))(nth 1 t1_draw_uklon)) (list(nth 0 (car spisokv))(nth 1 t2_draw_uklon))))))
      (repeat n
	(setq p1 (car spisokv))
	(setq spisokv (cdr spisokv))
	(setq p2 (car spisokv))
	(setq s (- (nth 0 p2) (nth 0 p1)))
	(if (> s 0)
	  (progn
	    (setq pro(rtos (abs(*(/ (* (- (nth 1 p1) (nth 1 p2))(/ (* mv (/ 1000.0 mg)) 1000.0)) (- (nth 0 p1) (nth 0 p2))) pro!)) 2 n_00))
	    (setq t1 (list (nth 0 p1)(nth 1 t1_draw_uklon)))
	    (setq t2 (list (nth 0 p2)(nth 1 t1_draw_uklon)))
	    (setq H1 (nth 1 p1))
	    (setq H2 (nth 1 p2))
	    (setq t11 (mapcar '+ (list(nth 0 p1)(nth 1 t1_draw_uklon)) (list 0 hs)))
	    (setq t1- (mapcar '+ (list(nth 0 p1)(nth 1 t1_draw_uklon)) (list 0 (/ hs 2))))
	    (setq t22 (mapcar '+ (list(nth 0 p2)(nth 1 t1_draw_uklon)) (list 0 hs)))
	    (setq t2- (mapcar '+ (list(nth 0 p2)(nth 1 t1_draw_uklon)) (list 0 (/ hs 2))))
	    (cond
	      ((= H1 H2); -
	       (setq obj-list(append obj-list (list (draw-line-vla model_spece t1- t2-))))
	       (if
		 (>(*(+ 1.6 (* 1.6 (/ 2.0 3.0))) scale-coefficient)s); условие
		 (progn
		   (setq t3 (mapcar '+ t11 (list (/(distance t1 t2)2) (/ (distance t1- t11) -4) )))
		   (setq obj-list(append obj-list(list(draw_mleader_vla model_spece pro(/ 2.5 (/ 1000.0 mg)) t3(mapcar '+ t3 (list (/ 2 (/ 1000.0 mg)) (/ 7 (/ 1000.0 mg))))))))
		   )
		 (progn
		   (if
		     (< (+(calculation_str_long_be_h_stile pro ht styl)(/ (* 1.6(/ 2.0 3.0)) (/ 1000.0 mg))) s)
		     (progn
		       (setq ht (/ 3.5 (/ 1000.0 mg)))
		       (setq ang 0)
		       )
		     (progn
		       (setq ang (/ pi 2))
		       (if
			 (< (+(/ 3.5 (/ 1000.0 mg))(/ (* 3.5 0.6) (/ 1000.0 mg)))s)
			 (setq ht (/ 3.5 (/ 1000.0 mg)))
			 (setq ht (* (distance t1- t2-) 0.6))
			 )
		       )
		     ); if
		   (setq obj-list
			  (append obj-list
				  (list
				    (draw-text-vla-by-list-property
				      (list(cons "spece" model_spece)
					   (cons "TextString" pro)
					   (cons "InsertionPoint" (mapcar '+ t1- (list (/ s 2) (/ hs 4) )))
					   (cons "Height" ht)
					   (cons "Rotation" ang)
					   
					   (cons "Alignment" 4)
					   (cons "StyleName" curent-text-style)
					   (cons "Linetype" "Continuous")
					   ))
				    )))
		   
		   ); progn
		 ); if
	       (if
		 (>(*(+ 1.6 (* 1.6 (/ 2.0 3.0))) scale-coefficient)s); условие
		 (progn
		   (setq t3 (mapcar '+ t1 (list (/(distance t1 t2)2) (/ (distance t1 t1-) 4) )))
		   (setq obj-list(append obj-list(list(draw_mleader_vla model_spece (rtos S 2 dec)(/ 2.5 (/ 1000.0 mg)) t3(mapcar '+ t3 (list (/ 2.0 (/ 1000.0 mg)) (/ -7.0 (/ 1000.0 mg))))))))
		   )
		 (progn
		   (if
		     (< (+(calculation_str_long_be_h_stile (rtos S 2 dec) ht styl)(/ (* 1.6(/ 2.0 3.0)) (/ 1000.0 mg))) (distance t1 t2))
		     (progn
		       (setq ht (/ 3.5 (/ 1000.0 mg)))
		       (setq ang 0)
		       )
		     (progn
		       (setq ang (/ pi 2))
		       (if
			 (< (+(/ 3.5 (/ 1000.0 mg))(/ (* 3.5 0.6) (/ 1000.0 mg)))(distance t1 t2))
			 (setq ht (/ 3.5 (/ 1000.0 mg)))
			 (setq ht (* (distance t1 t2) 0.6))
			 )
		       )
		     ); if
		   (setq obj-list
			  (append obj-list
				  (list
				    (draw-text-vla-by-list-property
				      (list(cons "spece" model_spece)
					   (cons "TextString" (rtos S 2 dec))
					   (cons "InsertionPoint" (mapcar '+ t1 (list (/(distance t1 t2)2) (/ (distance t1 t1-) 2) )))
					   (cons "Height" ht)
					   (cons "Rotation" ang)
					   
					   (cons "Alignment" 4)
					   (cons "StyleName" curent-text-style)
					   (cons "Linetype" "Continuous")
					   ))
				    )))
		   ); progn
		 ); if
	       )
	      ((> H1 H2); \
	       (setq obj-list(append obj-list (list (draw-line-vla model_spece t11 t2))))
	       (if (>(calculation_h_text_in_triang t1 t11 t2 (calculation_text_ratio_h_for_dlin (rtos S 2 dec) ht styl))
		     (calculation_h_text_in_triang t1 t2 t11 (calculation_text_ratio_h_for_dlin (rtos S 2 dec) ht styl))
		     )
		 (cond
		   ((> (*(calculation_h_text_in_triang t1 t11 t2 (calculation_text_ratio_h_for_dlin (rtos S 2 dec) ht styl)) 0.8) (/ 3.5 (/ 1000.0 mg)))
		    (setq obj-list
			   (append obj-list
				   (list
				     (draw-text-vla-by-list-property
				       (list(cons "spece" model_spece)
					    (cons "TextString" (rtos S 2 dec))
					    (cons "InsertionPoint" (mapcar '+ t1 (list (/(/ 3.5 (/ 1000.0 mg))3) (/(/ 3.5 (/ 1000.0 mg))3) )))
					    (cons "Height" (/ 3.5 (/ 1000.0 mg)))
					    (cons "Rotation" 0)
					    (cons "Alignment" 0)
					    (cons "StyleName" curent-text-style)
					    (cons "Linetype" "Continuous")
					    ))
				     )))
		    )
		   ((< (*(calculation_h_text_in_triang t1 t11 t2 (calculation_text_ratio_h_for_dlin (rtos S 2 dec) ht styl)) 0.8) (/ 1.6 (/ 1000.0 mg)))
		    (setq t3 (mapcar '+ t1 (list (/(distance t1 t2)2) (/ 1 (/ 1000.0 mg)) )))
		    (setq obj-list(append obj-list(list(draw_mleader_vla model_spece (rtos S 2 dec)(/ 2.5 (/ 1000.0 mg)) t3(mapcar '+ t3 (list (/ 2 (/ 1000.0 mg)) (/ -7 (/ 1000.0 mg))))))))
		    )
		   (T
		    (setq obj-list
			   (append obj-list
				   (list
				     (draw-text-vla-by-list-property
				       (list(cons "spece" model_spece)
					    (cons "TextString" (rtos S 2 dec))
					    (cons "InsertionPoint" (mapcar '+ t1 (list
										   (/(*(calculation_h_text_in_triang t1 t11 t2 (calculation_text_ratio_h_for_dlin (rtos S 2 dec) ht styl)) 0.8)3)
										   (/(*(calculation_h_text_in_triang t1 t11 t2 (calculation_text_ratio_h_for_dlin (rtos S 2 dec) ht styl)) 0.8)3)
										   )))
					    (cons "Height" (*(calculation_h_text_in_triang t1 t11 t2 (calculation_text_ratio_h_for_dlin (rtos S 2 dec) ht styl)) 0.8))
					    (cons "Rotation" (/ pi 2))
					    (cons "Alignment" 6);*********************************
					    (cons "StyleName" curent-text-style)
					    (cons "Linetype" "Continuous")
					    ))
				     )))
		    )
		   )
		 (cond
		   ((> (*(calculation_h_text_in_triang t1 t2 t11 (calculation_text_ratio_h_for_dlin (rtos S 2 dec) ht styl)) 0.8) (/ 3.5 (/ 1000.0 mg)))
		    (setq obj-list
			   (append obj-list
				   (list
				     (draw-text-vla-by-list-property
				       (list(cons "spece" model_spece)
					    (cons "TextString" (rtos S 2 dec))
					    (cons "InsertionPoint" (mapcar '+ t1 (list (/(/ 3.5 (/ 1000.0 mg))3) (/(/ 3.5 (/ 1000.0 mg))3) )))
					    (cons "Height" (/ 3.5 (/ 1000.0 mg)))
					    (cons "Rotation" (/ pi 2))
					    
					    (cons "Alignment" 6)
					    (cons "StyleName" curent-text-style)
					    (cons "Linetype" "Continuous")
					    ))
				     )))
		    )
		   ((< (*(calculation_h_text_in_triang t1 t2 t11 (calculation_text_ratio_h_for_dlin (rtos S 2 dec) ht styl)) 0.8) (/ 1.6 (/ 1000.0 mg)))
		    
		    (setq t3 (mapcar '+ t1 (list (/(distance t1 t2)2) (/ 1 (/ 1000.0 mg)) )))
		    (setq obj-list(append obj-list(list(draw_mleader_vla model_spece (rtos S 2 dec)(/ 2.5 (/ 1000.0 mg)) t3(mapcar '+ t3 (list (/ 2 (/ 1000.0 mg)) (/ -7 (/ 1000.0 mg))))))))
		    )
		   (T
		    (setq obj-list
			   (append obj-list
				   (list
				     (draw-text-vla-by-list-property
				       (list(cons "spece" model_spece)
					    (cons "TextString" (rtos S 2 dec))
					    (cons "InsertionPoint" (mapcar '+ t1 (list
										   (/(*(calculation_h_text_in_triang t1 t2 t11 (calculation_text_ratio_h_for_dlin (rtos S 2 dec) ht styl)) 0.8)3)
										   (/(*(calculation_h_text_in_triang t1 t2 t11 (calculation_text_ratio_h_for_dlin (rtos S 2 dec) ht styl)) 0.8)3)
										   )))
					    (cons "Height" (*(calculation_h_text_in_triang t1 t2 t11 (calculation_text_ratio_h_for_dlin (rtos S 2 dec) ht styl)) 0.8))
					    (cons "Rotation" (/ pi 2))
					    
					    (cons "Alignment" 6)
					    (cons "StyleName" curent-text-style)
					    (cons "Linetype" "Continuous")
					    ))
				     )))
		    )
		   )
		 )
	       (if (>(calculation_h_text_in_triang t22 t2 t11 (calculation_text_ratio_h_for_dlin pro ht styl))
		     (calculation_h_text_in_triang t22 t11 t2 (calculation_text_ratio_h_for_dlin pro ht styl))
		     )
		 (cond
		   ((> (*(calculation_h_text_in_triang t22 t2 t11 (calculation_text_ratio_h_for_dlin pro ht styl)) 0.8) (/ 3.5 (/ 1000.0 mg)))
		    (setq obj-list
			   (append obj-list
				   (list
				     (draw-text-vla-by-list-property
				       (list(cons "spece" model_spece)
					    (cons "TextString" pro)
					    (cons "InsertionPoint" (mapcar '- t22 (list (/(/ 3.5 (/ 1000.0 mg))3) (/(/ 3.5 (/ 1000.0 mg))3) )))
					    (cons "Height" (/ 3.5 (/ 1000.0 mg)))
					    (cons "Rotation" 0)
					    
					    (cons "Alignment" 8)
					    (cons "StyleName" curent-text-style)
					    (cons "Linetype" "Continuous")
					    ))
				     )))
		    )
		   ((< (*(calculation_h_text_in_triang t22 t2 t11 (calculation_text_ratio_h_for_dlin pro ht styl)) 0.8) (/ 1.6 (/ 1000.0 mg)))
		    (setq t3 (mapcar '- t22 (list (/(distance t1 t2)2) (/ 1 (/ 1000.0 mg)) )))
		    (setq obj-list(append obj-list(list(draw_mleader_vla model_spece pro(/ 2.5 (/ 1000.0 mg)) t3(mapcar '+ t3 (list (/ 2 (/ 1000.0 mg)) (/ 7 (/ 1000.0 mg))))))))
		    )
		   (T
		    (setq obj-list
			   (append obj-list
				   (list
				     (draw-text-vla-by-list-property
				       (list(cons "spece" model_spece)
					    (cons "TextString" pro)
					    (cons "InsertionPoint" (mapcar '- t22 (list
										    (/(*(calculation_h_text_in_triang t22 t2 t11 (calculation_text_ratio_h_for_dlin pro ht styl)) 0.8)3)
										    (/(*(calculation_h_text_in_triang t22 t2 t11 (calculation_text_ratio_h_for_dlin pro ht styl)) 0.8)3)
										    )))
					    (cons "Height" (*(calculation_h_text_in_triang t22 t2 t11 (calculation_text_ratio_h_for_dlin pro ht styl)) 0.8))
					    (cons "Rotation" 0)
					    
					    (cons "Alignment" 8)
					    (cons "StyleName" curent-text-style)
					    (cons "Linetype" "Continuous")
					    ))
				     )))
		    )
		   )
		 (cond
		   ((> (*(calculation_h_text_in_triang t22 t11 t2 (calculation_text_ratio_h_for_dlin pro ht styl)) 0.8) (/ 3.5 (/ 1000.0 mg)))
		    (setq obj-list
			   (append obj-list
				   (list
				     (draw-text-vla-by-list-property
				       (list(cons "spece" model_spece)
					    (cons "TextString" pro)
					    (cons "InsertionPoint" (mapcar '- t22 (list (/(/ 3.5 (/ 1000.0 mg))3) (/(/ 3.5 (/ 1000.0 mg))3) )))
					    (cons "Height" (/ 3.5 (/ 1000.0 mg)))
					    (cons "Rotation" (/ pi 2))
					    
					    (cons "Alignment" 2)
					    (cons "StyleName" curent-text-style)
					    (cons "Linetype" "Continuous")
					    ))
				     )))
		    )
		   ((< (*(calculation_h_text_in_triang t22 t11 t2 (calculation_text_ratio_h_for_dlin pro ht styl)) 0.8) (/ 1.6 (/ 1000.0 mg)))
		    (setq t3 (mapcar '- t22 (list (/(distance t1 t2)2) (/ 1 (/ 1000.0 mg)) )))
		    (setq obj-list(append obj-list(list(draw_mleader_vla model_spece pro(/ 2.5 (/ 1000.0 mg)) t3(mapcar '+ t3 (list (/ 2 (/ 1000.0 mg)) (/ 7 (/ 1000.0 mg))))))))
		    )
		   (T
		    (setq obj-list
			   (append obj-list
				   (list
				     (draw-text-vla-by-list-property
				       (list(cons "spece" model_spece)
					    (cons "TextString" pro)
					    (cons "InsertionPoint" (mapcar '- t22 (list
										    (/(*(calculation_h_text_in_triang t22 t11 t2 (calculation_text_ratio_h_for_dlin pro ht styl)) 0.8)3)
										    (/(*(calculation_h_text_in_triang t22 t11 t2 (calculation_text_ratio_h_for_dlin pro ht styl)) 0.8)3)
										    )))
					    (cons "Height" (*(calculation_h_text_in_triang t22 t11 t2 (calculation_text_ratio_h_for_dlin pro ht styl)) 0.8))
					    (cons "Rotation" (/ pi 2))
					    
					    (cons "Alignment" 2)
					    (cons "StyleName" curent-text-style)
					    (cons "Linetype" "Continuous")
					    ))
				     )))
		    )
		   )
		 )
	       )
	      ((< H1 H2); /
	       (setq obj-list(append obj-list (list (draw-line-vla model_spece t1 t22))))
	       (if (>(calculation_h_text_in_triang t2 t22 t1 (calculation_text_ratio_h_for_dlin (rtos S 2 dec) ht styl))
		     (calculation_h_text_in_triang t2 t1 t22 (calculation_text_ratio_h_for_dlin (rtos S 2 dec) ht styl))
		     )
		 (cond
		   ((> (*(calculation_h_text_in_triang t2 t22 t1 (calculation_text_ratio_h_for_dlin (rtos S 2 dec) ht styl)) 0.8) (/ 3.5 (/ 1000.0 mg)))
		    (setq obj-list
			   (append obj-list
				   (list
				     (draw-text-vla-by-list-property
				       (list(cons "spece" model_spece)
					    (cons "TextString" (rtos S 2 dec))
					    (cons "InsertionPoint" (mapcar '+ t2 (list (/(/ -3.5 (/ 1000.0 mg))3) (/(/ 3.5 (/ 1000.0 mg))3) )))
					    (cons "Height" (/ 3.5 (/ 1000.0 mg)))
					    (cons "Rotation" 0)
					    
					    (cons "Alignment" 2)
					    (cons "StyleName" curent-text-style)
					    (cons "Linetype" "Continuous")
					    ))
				     )))
		    )
		   ((< (*(calculation_h_text_in_triang t2 t22 t1 (calculation_text_ratio_h_for_dlin (rtos S 2 dec) ht styl)) 0.8) (/ 1.6 (/ 1000.0 mg)))
		    (setq t3 (mapcar '+ t2 (list (/(distance t1 t2)2) (/ -1 (/ 1000.0 mg)) )))
		    (setq obj-list(append obj-list(list(draw_mleader_vla model_spece (rtos S 2 dec)(/ 2.5 (/ 1000.0 mg)) t3(mapcar '+ t3 (list (/ 2 (/ 1000.0 mg)) (/ -7 (/ 1000.0 mg))))))))
		    )
		   (T
		    (setq obj-list
			   (append obj-list
				   (list
				     (draw-text-vla-by-list-property
				       (list(cons "spece" model_spece)
					    (cons "TextString" (rtos S 2 dec))
					    (cons "InsertionPoint" (mapcar '+ t2 (list
										   (/(*(calculation_h_text_in_triang t2 t22 t1 (calculation_text_ratio_h_for_dlin (rtos S 2 dec) ht styl)) -0.8)3)
										   (/(*(calculation_h_text_in_triang t2 t22 t1 (calculation_text_ratio_h_for_dlin (rtos S 2 2) ht styl)) 0.8)3)
										   )))
					    (cons "Height" (*(calculation_h_text_in_triang t2 t22 t1 (calculation_text_ratio_h_for_dlin (rtos S 2 2) ht styl)) 0.8))
					    (cons "Rotation" 0)
					    
					    (cons "Alignment" 2)
					    (cons "StyleName" curent-text-style)
					    (cons "Linetype" "Continuous")
					    ))
				     )))
		    )
		   )
		 (cond
		   ((> (*(calculation_h_text_in_triang t2 t1 t22 (calculation_text_ratio_h_for_dlin (rtos S 2 2) ht styl)) 0.8) (/ 3.5 (/ 1000.0 mg)))
		    (setq obj-list
			   (append obj-list
				   (list
				     (draw-text-vla-by-list-property
				       (list(cons "spece" model_spece)
					    (cons "TextString" (rtos S 2 dec))
					    (cons "InsertionPoint" (mapcar '+ t2 (list (/(/ -3.5 (/ 1000.0 mg))3) (/(/ 3.5 (/ 1000.0 mg))3) )))
					    (cons "Height" (/ 3.5 (/ 1000.0 mg)))
					    (cons "Rotation" (/ pi 2))
					    
					    (cons "Alignment" 0)
					    (cons "StyleName" curent-text-style)
					    (cons "Linetype" "Continuous")
					    ))
				     )))
		    )
		   ((< (*(calculation_h_text_in_triang t2 t1 t22 (calculation_text_ratio_h_for_dlin (rtos S 2 2) ht styl)) 0.8) (/ 1.6 (/ 1000.0 mg)))
		    (setq t3 (mapcar '- t2 (list (/(distance t1 t2)2) (/ -1 (/ 1000.0 mg)) )))
		    (setq obj-list(append obj-list(list(draw_mleader_vla model_spece (rtos S 2 dec)(/ 2.5 (/ 1000.0 mg)) t3(mapcar '+ t3 (list (/ 2 (/ 1000.0 mg)) (/ -7 (/ 1000.0 mg))))))))
		    )
		   (T
		    (setq obj-list
			   (append obj-list
				   (list
				     (draw-text-vla-by-list-property
				       (list(cons "spece" model_spece)
					    (cons "TextString" (rtos S 2 dec))
					    (cons "InsertionPoint" (mapcar '+ t2 (list
										   (/(*(calculation_h_text_in_triang t2 t1 t22 (calculation_text_ratio_h_for_dlin (rtos S 2 dec) ht styl)) -0.8)3)
										   (/(*(calculation_h_text_in_triang t2 t1 t22 (calculation_text_ratio_h_for_dlin (rtos S 2 dec) ht styl)) 0.8)3)
										   )))
					    (cons "Height" (*(calculation_h_text_in_triang t2 t1 t22 (calculation_text_ratio_h_for_dlin (rtos S 2 dec) ht styl)) 0.8))
					    (cons "Rotation" (/ pi 2))
					    
					    (cons "Alignment" 0)
					    (cons "StyleName" curent-text-style)
					    (cons "Linetype" "Continuous")
					    ))
				     )))
		    )
		   )
		 )
	       (if (>(calculation_h_text_in_triang t11 t1 t22 (calculation_text_ratio_h_for_dlin pro ht styl))
		     (calculation_h_text_in_triang t11 t22 t1 (calculation_text_ratio_h_for_dlin pro ht styl))
		     )
		 (cond
		   ((> (*(calculation_h_text_in_triang t11 t1 t22 (calculation_text_ratio_h_for_dlin pro ht styl)) 0.8) (/ 3.5 (/ 1000.0 mg)))
		    (setq obj-list
			   (append obj-list
				   (list
				     (draw-text-vla-by-list-property
				       (list(cons "spece" model_spece)
					    (cons "TextString" pro)
					    (cons "InsertionPoint" (mapcar '+ t11 (list (/(/ 3.5 (/ 1000.0 mg))3) (/(/ -3.5 (/ 1000.0 mg))3) )))
					    (cons "Height" (/ 3.5 (/ 1000.0 mg)))
					    (cons "Rotation" 0)
					    
					    (cons "Alignment" 6)
					    (cons "StyleName" curent-text-style)
					    (cons "Linetype" "Continuous")
					    ))
				     )))
		    
		    
		    
		    
		    )
		   ((< (*(calculation_h_text_in_triang t11 t1 t22 (calculation_text_ratio_h_for_dlin pro ht styl)) 0.8) (/ 1.6 (/ 1000.0 mg)))
		    (setq t3 (mapcar '+ t11 (list (/(distance t1 t2)2) (/ -1 (/ 1000.0 mg)) )))
		    (setq obj-list(append obj-list(list(draw_mleader_vla model_spece pro(/ 2.5 (/ 1000.0 mg)) t3(mapcar '+ t3 (list (/ 2 (/ 1000.0 mg)) (/ 7 (/ 1000.0 mg))))))))
		    )
		   (T
		    (setq obj-list
			   (append obj-list
				   (list
				     (draw-text-vla-by-list-property
				       (list(cons "spece" model_spece)
					    (cons "TextString" pro)
					    (cons "InsertionPoint" (mapcar '+ t11 (list
										    (/(*(calculation_h_text_in_triang t11 t1 t22 (calculation_text_ratio_h_for_dlin pro ht styl)) 0.8)3)
										    (/(*(calculation_h_text_in_triang t11 t1 t22 (calculation_text_ratio_h_for_dlin pro ht styl)) -0.8)3)
										    )))
					    (cons "Height" (*(calculation_h_text_in_triang t11 t1 t22 (calculation_text_ratio_h_for_dlin pro ht styl)) 0.8))
					    (cons "Rotation" 0)
					    
					    (cons "Alignment" 6)
					    (cons "StyleName" curent-text-style)
					    (cons "Linetype" "Continuous")
					    ))
				     )))
		    )
		   )
		 (cond
		   ((> (*(calculation_h_text_in_triang t11 t22 t1 (calculation_text_ratio_h_for_dlin pro ht styl))0.8)(/ 3.5 (/ 1000.0 mg)))
		    (setq obj-list
			   (append obj-list
				   (list
				     (draw-text-vla-by-list-property
				       (list(cons "spece" model_spece)
					    (cons "TextString" pro)
					    (cons "InsertionPoint" (mapcar '+ t11 (list (/(/ 3.5 (/ 1000.0 mg))3) (/(/ -3.5 (/ 1000.0 mg))3) )))
					    (cons "Height" (/ 3.5 (/ 1000.0 mg)))
					    (cons "Rotation" (/ pi 2))
					    
					    (cons "Alignment" 8)
					    (cons "StyleName" curent-text-style)
					    (cons "Linetype" "Continuous")
					    ))
				     )))
		    )
		   ((< (*(calculation_h_text_in_triang t11 t22 t1 (calculation_text_ratio_h_for_dlin pro ht styl)) 0.8) (/ 1.6 (/ 1000.0 mg)))
		    (setq t3 (mapcar '+ t11 (list (/(distance t1 t2)2) (/ -1 (/ 1000.0 mg)) )))
		    (setq obj-list(append obj-list(list(draw_mleader_vla model_spece pro(/ 2.5 (/ 1000.0 mg)) t3(mapcar '+ t3 (list (/ 2 (/ 1000.0 mg)) (/ 7 (/ 1000.0 mg))))))))
		    )
		   (T
		    (setq obj-list
			   (append obj-list
				   (list
				     (draw-text-vla-by-list-property
				       (list(cons "spece" model_spece)
					    (cons "TextString" pro)
					    (cons "InsertionPoint" (mapcar '+ t11 (list
										    (/(*(calculation_h_text_in_triang t11 t22 t1 (calculation_text_ratio_h_for_dlin pro ht styl))0.8)3)
										    (/(*(calculation_h_text_in_triang t11 t22 t1 (calculation_text_ratio_h_for_dlin pro ht styl))-0.8)3)
										    )))
					    (cons "Height" (*(calculation_h_text_in_triang t11 t22 t1 (calculation_text_ratio_h_for_dlin pro ht styl)) 0.8))
					    (cons "Rotation" (/ pi 2))
					    
					    (cons "Alignment" 8)
					    (cons "StyleName" curent-text-style)
					    (cons "Linetype" "Continuous")
					    ))
				     )))
		    
		    )
		   )
		 )
	       )
	      )
	    (setq obj-list
		   (append obj-list
			   (list
			     (draw-line-vla-by-list-property
			       (list
				 (cons "spece" model_spece)
				 (cons "StartPoint" (list(nth 0 p2)(nth 1 t1_draw_uklon)))
				 (cons "EndPoint" (list(nth 0 p2)(nth 1 t2_draw_uklon)))
				 (cons "Linetype" "Continuous")
				 ))
			     )))
	    )
	  )
	)
;;;      (if(=(vlax-ldata-get "geo_tools_dictionary" "profile-mode")1)
;;;	(progn
;;;	  (if (eq (type(nth 10 list-select-string)) 'LIST)
;;;	    (foreach x (nth 10 list-select-string)
;;;	      (if x
;;;		(if(not(vlax-erased-p x))(vla-delete x))
;;;		)
;;;	      )
;;;	    )
;;;	  (vlax-ldata-put block-profile-obj "profile-string-list"
;;;	    (replase-item-in-list-by-number (replase-item-in-list-by-number obj-list 10 list-select-string) number-string profile-string-list)
;;;	    )
;;;	  ))
      
      )
    )
  )