(defun C:inport_text_file_to_3dpoly ( / flag list-string n put-text-file item flag-length first-pk item-temp list-data raw-list-string flag-error point angl list-points)
  ; ������ ���������� ����� � ������������ ��������� � ������ � 3d ���������
  (geo_tools-help "inport-text-file-to-3dpoly")
  (if(setq put-text-file (getfiled "����� �����" (getvar "DWGPREFIX") "txt" 0))
    (progn
      (setq raw-list-string (file_read_to_list put-text-file))
      (setq list-string(clear-replase-string-inport-text-file-to-3dpoly raw-list-string))
      (setq n 0)
      (while (not flag)
	(if (chek-text-to-sym-inport-text-file-to-3dpoly (nth n list-string))
	  (progn
	    (setq item(convert-string-to-list-be-delimiter (nth n list-string) " "))
	    (cond
	      ((<(length item)2)
	       (alert (strcat "������! �� ���������� ������ � ������ #"(itoa (1+ n))"\n�������:\n"(nth n raw-list-string)"\n����������:\n"(nth n list-string)))
	       (setq flag T)(setq flag-error T)
	       )
	      ((>(length item)3)
	       (alert (strcat "������! ������� ����� ������ � ������ #"(itoa (1+ n))"\n�������:\n"(nth n raw-list-string)"\n����������:\n"(nth n list-string)))
	       (setq flag T)(setq flag-error T)
	       )
	      (T
	       (if (= n 0)
		 (if (vl-string-search "+"(nth 0 item))
		   (setq flag-length "������"
			 first-pk (convert-pk-string-to-distance(nth 0 item)))
		   (setq flag-length "�����"))
		 
		 (if (vl-string-search "+"(nth 0 item))
		   (if (= flag-length "�����")
		     (progn
		       (alert (strcat "������! ����� ���� ������ � ������ #"(itoa (1+ n))"\n�������:\n"(nth n raw-list-string)"\n����������:\n"(nth n list-string)))
		       (setq flag T)(setq flag-error T)
		       ))
		   (if (= flag-length "������")
		     (progn
		       (alert (strcat "������! ����� ���� ������ � ������ #"(itoa (1+ n))"\n�������:\n"(nth n raw-list-string)"\n����������:\n"(nth n list-string)))
		       (setq flag T)(setq flag-error T)
		       ))
		   )
		 )
	       (if (not flag)
		 (if (vl-string-search "+"(nth 0 item))
		   (progn
		     (setq item-temp(list(- (convert-pk-string-to-distance (nth 0 item))first-pk)(atof(nth 1 item))))
		     (setq first-pk (convert-pk-string-to-distance(nth 0 item)))
		     (if (nth 2 item)(setq item-temp (append item-temp (list (atof(nth 2 item))))))
		     (setq list-data (append list-data (list item-temp)))
		     )
		   (progn
		     (setq item-temp(list(atof(nth 0 item))(atof(nth 1 item))))
		     (if (nth 2 item)(setq item-temp (append item-temp (list (atof(nth 2 item))))))
		     (setq list-data (append list-data (list item-temp)))
		     )
		   )
		 )
	       )
	      )
	    )
	  (progn
	    (alert (strcat "������! �� ���������� ������� � ������ #"(itoa (1+ n))"\n�������:\n"(nth n raw-list-string)"\n����������:\n"(nth n list-string)))
	    (setq flag T)(setq flag-error T)
	    )
	  )
	(setq n (1+ n))
	(if (= n (length list-string)) (setq flag T))
	)
      (if (not flag-error)
	(if (setq point (getpoint "\n����� ��������� ����� ���������: "))
	  (progn
	    (setq angl (/ pi 2))
	    (foreach x list-data
	      
	      (setq point(polar point angl (nth 0 x)))
	      (if (nth 2 x)
		(setq angl (- angl (convert-grad-rad(nth 2 x)))))
	      (setq point (list (nth 0 point)(nth 1 point)(nth 1 x)))
	      (setq list-points (append list-points(list point)))
	      )
	    (vla-add3dPoly model_spece
	      (vlax-safearray-fill
		(vlax-make-safearray vlax-vbDouble (cons 0 (1-(*(length list-points)3))))
		(apply 'append (mapcar 'list (mapcar 'car list-points)
				       (mapcar 'cadr list-points)
				       (mapcar 'caddr list-points)))))
	    )
	  )
	)
      )
    )
  (princ)
  )


(defun clear-replase-string-inport-text-file-to-3dpoly (input-list / result-list)
  ; ������� ������ �� ��������� �������� ������� ��������, ������� �����,
  ; ������ ������ ��������� �� ������ ������� �� �����
  (foreach x input-list
    (if x
      (setq result-list (append result-list(list
					     (vl-string-trim " "
					       (edit-string-replase-str "." ".."
						 (edit-string-replase-str "." ","
						   (edit-string-replase-str " " "  "
						     (edit-string-replase-str " " "\\t"
						       x)))))
					     ))))
    )
  result-list
  )

(defun chek-text-to-sym-inport-text-file-to-3dpoly (string / )
  ; �������� ������ �� �� ���������� ������� (��� ����� �������� ���� + - .)
  (cond
    ((wcmatch string "*@*")
     nil
     )
    ((wcmatch string "*.*")
     (if (wcmatch (edit-string-replase-str "" " "(edit-string-replase-str "" "."(edit-string-replase-str "" "-"(edit-string-replase-str "" "+" string)))) "*.*")
       nil
       T)
     )
    (T
     T)
    )
  )