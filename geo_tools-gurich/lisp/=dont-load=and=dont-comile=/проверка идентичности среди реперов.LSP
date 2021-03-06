(defun C:ddd ( / collection list-item-all list-item-new-coor list-item-no-xy-coor n name-punkt number o-doc pos punkt-recorg result)
  ;�������������� ���� ������� ���������� ����� - � ()
  (terpri)
  (setq o-doc(XML-Get-XMLObject (strcat(getvar "DWGPREFIX")"������� ������� ���������.xml")))
  (setq list-item-no-xy-coor(XML-Get-Child-ByAttribute o-doc nil "name" "no-xy-coor"))
  (setq list-item-all(XML-Get-Child-ByAttribute o-doc nil "name" "all"))
  (setq list-item-new-coor(XML-Get-Child-ByAttribute o-doc nil "name" "new-coor"))
  (setq collection (XML-Get-Children list-item-all nil))
  (setq n 0)
  (setq number 1000001)
  (while (setq punkt-recorg (vlax-get-property collection 'item n))
    (grtext -1 (strcat(itoa n)"/"(itoa(vlax-get-property collection 'length))))
    (setq name-punkt(XML-Get-Attribute punkt-recorg "name" ""))
    (cond
      ((wcmatch name-punkt "@*,?@*")
       (XML-Put-Attribute punkt-recorg "name" (itoa number))
       (XML-Put-Attribute punkt-recorg "comment" (strcat "������������� �: "name-punkt))
       (setq result(append result(list(strcat name-punkt"=>"(itoa number)))))
       (setq number (1+ number))
       )
      ((wcmatch name-punkt "* @*")
       (setq pos(vl-string-search " " name-punkt))
       (XML-Put-Attribute punkt-recorg "name" (substr name-punkt 1 pos))
       (XML-Put-Attribute punkt-recorg "comment" (strcat "������������� �: "name-punkt))
       (setq result(append result(list(strcat name-punkt"=>"(substr name-punkt 1 pos)))))
       )
      ((wcmatch name-punkt "*@*")
       (XML-Put-Attribute punkt-recorg "name" (itoa number))
       (XML-Put-Attribute punkt-recorg "comment" (strcat "������������� �: "name-punkt))
       (setq result(append result(list(strcat name-punkt"=>"(itoa number)))))
       (setq number (1+ number))
       )
      ((setq pos(vl-string-search "-" name-punkt))
       (XML-Put-Attribute punkt-recorg "name" (substr name-punkt 1 pos))
       (XML-Put-Attribute punkt-recorg "comment" (strcat "������������� �: "name-punkt))
       (setq result(append result(list(strcat name-punkt"=>"(substr name-punkt 1 pos)))))
       )
      ((setq pos(vl-string-search "(" name-punkt))
       (XML-Put-Attribute punkt-recorg "name" (substr name-punkt 1 pos))
       (XML-Put-Attribute punkt-recorg "comment" (strcat "������������� �: "name-punkt))
       (setq result(append result(list(strcat name-punkt"=>"(substr name-punkt 1 pos)))))
       )
      )
    (XML-Put-Attribute punkt-recorg "name" (vl-string-left-trim "0" (XML-Get-Attribute punkt-recorg "name" "")))
    (setq n (1+ n))
    )
  (if result
    (file_write_list_to_file (strcat(getvar "DWGPREFIX")"raport.txt") result)
    )
  (XML-Save o-doc)
  (vlax-release-object list-item-all)
  (vlax-release-object list-item-no-xy-coor)
  (vlax-release-object o-doc)
  (grtext -1 "")
  (princ)
  )

;;;(setq n 0)
;;;(while (< 10000000) (acet-ui-progress-safe n)(setq n (1+ n)))
;;;(setq kn 10000000)
;;;(acet-ui-progress-init "������������ �����" kn)
;;;(while (> kn 0)
;;;  (setq n (1+ n))
;;;  (acet-ui-progress-safe n)
;;;  ;; �������
;;;  );;end while
;;;(acet-ui-progress-done)

;;;(setq i 0)
;;;(repeat 10000
;;;  (grtext -1 (strcat "������� - " (itoa (setq i (1+ i)))))
;;;  (repeat 200 (princ)) ;_���-�� ������
;;;  )
;;;(command "_redrawall")

;;; (grtext -1 (strcat "������� - " (itoa (setq i (1+ i)))))
;;;;���, � ����������� �� �������� 
;;;(acet-ui-progress-safe n)(setq n (1+ n))

(defun C:eee ( / collection-new-coor collection-old-coor comment-old list-item-all list-item-new-coor list-item-no-xy-coor n
	      name-new name-old nn o-doc punkt-new punkt-old punkt-recorg x-new x-old y-new y-old year-old)
    ;���������� ��� ������� XML � ��������, ������ ������ ��� ������� � �������...
  (terpri)
  (princ "��������. ���...")
  (if(setq o-doc(XML-Get-XMLObject (strcat(getvar "DWGPREFIX")"������� ������� ���������.xml")))
    (progn
      (setq list-item-no-xy-coor(XML-Get-Child-ByAttribute o-doc nil "name" "no-xy-coor"))
      (setq list-item-all(XML-Get-Child-ByAttribute o-doc nil "name" "all"))
      (setq list-item-new-coor(XML-Get-Child-ByAttribute o-doc nil "name" "new-coor"))
      (setq collection-new-coor (XML-Get-Children list-item-new-coor nil))
      (setq collection-old-coor (XML-Get-Children list-item-all nil))
      (setq n 0)
      (while (setq punkt-new (vlax-get-property collection-new-coor 'item n))
	(setq nn 0)
	(setq name-new (XML-Get-Attribute punkt-new "name" ""))
	(setq x-new (XML-Get-Attribute punkt-new "N" ""))
	(setq y-new (XML-Get-Attribute punkt-new "E" ""))
	(while (setq punkt-old (vlax-get-property collection-old-coor 'item nn))
	  (grtext -1 (strcat(itoa n)
			    "/"(itoa nn)
			    "/"(itoa(vlax-get-property collection-new-coor 'length))
			    "/"(itoa(vlax-get-property collection-old-coor 'length))))
	  (setq name-old (XML-Get-Attribute punkt-old "name" ""))
	  (setq x-old (XML-Get-Attribute punkt-old "N" ""))
	  (setq y-old (XML-Get-Attribute punkt-old "E" ""))
	  (setq year-old (if(XML-Get-Attribute punkt-old "year" "")(XML-Get-Attribute punkt-old "year" "")""))
	  (setq comment-old (if(XML-Get-Attribute punkt-old "comment" "")(XML-Get-Attribute punkt-old "comment" "")""))
	  (cond
	    ((and(= x-new x-old)
		 (= y-new y-old)
		 )
	     (setq result(append result(list (convert-punkt-recorg-to-string punkt-new))))
	     (XML-Remove-Child punkt-new)
	     (setq n (1- n))
	     (setq nn (+ nn 1000000))
	     (if (/= name-new name-old)
	       (if(= comment-old "")
		 (XML-Put-Attribute punkt-recorg "comment" (strcat "����� �� ��.������: "name-new))
		 (XML-Put-Attribute punkt-recorg "comment" (strcat comment-old"; ����� �� ��.������: "name-new))
		 ))
	     
	     )
	    ((equal (list (atof y-new)(atof x-new))
		    (list (atof y-old)(atof x-old))1.5)
	     (if (/= year-old "")
	       (progn
		 (setq result(append result(list (convert-punkt-recorg-to-string punkt-new))))
		 (XML-Remove-Child punkt-new)
		 (setq n (1- n))
		 (setq nn (+ nn 1000000))
		 (if(= comment-old "")
		   (XML-Put-Attribute punkt-recorg "comment" (strcat "����� �� ��.������: "name-new))
		   (XML-Put-Attribute punkt-recorg "comment" (strcat comment-old"; ����� �� ��.������: "name-new))
		   ))
	       )
	     )
	    );cond
	  (setq nn (1+ nn))
	  )
	(setq n (1+ n))
	(XML-Save o-doc)
	(if result(file_write_list_to_file (strcat(getvar "DWGPREFIX")"raport.txt") result))
	)
      (XML-Save o-doc)
      (vlax-release-object list-item-no-xy-coor)
      (vlax-release-object list-item-all)
      (vlax-release-object o-doc)
      )
    (alert "������! �������� ���� ��������� �� ���������!")
    )
  (princ)
  )

(defun C:ggg ( / comment-old list-item-new-coor list-item-old-coor list-new-coor list-old-coor n
	       name-new name-old nn o-doc punkt-new punkt-old x-new x-old y-new y-old year-old add-comment)
  ;���������� ��� ������� XML � ��������, ������ ������ ��� ������� � �������... ����� ���������� ��������� � �������������� ������
  (princ "\n��������. ���...")
  (if(setq o-doc(XML-Get-XMLObject (strcat(getvar "DWGPREFIX")"������� ������� ���������.xml")))
    (progn
      (setq list-item-old-coor(XML-Get-Child-ByAttribute o-doc nil "name" "all"))
      (setq list-item-new-coor(XML-Get-Child-ByAttribute o-doc nil "name" "new-coor"))
      (setq list-old-coor (convert-child-to-ass-list list-item-old-coor))
      (setq list-new-coor (convert-child-to-ass-list list-item-new-coor))
      (setq n 0)
      (while (setq punkt-new (nth n list-new-coor))
	(grtext -1 (strcat"����������� "(itoa n)"/"(itoa(length list-new-coor))))
	(setq nn 0)
	(setq name-new (cadr(assoc"name"punkt-new)))
	(setq x-new (cadr(assoc"N"punkt-new)))
	(setq y-new (cadr(assoc"E"punkt-new)))
	(while (setq punkt-old (nth nn list-old-coor))
	  (setq name-old nil
		x-old nil
		y-old nil
		year-old nil
		comment-old nil)
	  (setq name-old (cadr(assoc"name"punkt-old)))
	  (setq x-old (cadr(assoc"N"punkt-old)))
	  (setq y-old (cadr(assoc"E"punkt-old)))
	  (if(cadr(assoc"year"punkt-old))(setq year-old (cadr(assoc"year"punkt-old))))
	  (if(cadr(assoc"comment"punkt-old))(setq comment-old (cadr(assoc"comment"punkt-old))))
	  (cond
	    ((and(= x-new x-old)
		 (= y-new y-old)
		 )
	     (setq result(append result(list (strcat "������|"(convert-ass-list-to-string punkt-new)))))
	     (setq list-new-coor(vl-remove punkt-new list-new-coor))
	     (if (/= name-new name-old)
	       (progn
		 (if comment-old
		   (setq list-old-coor(subst(subst (list "comment" (strcat comment-old"; ����.: name="name-new))(assoc "comment" punkt-old) punkt-old)punkt-old list-old-coor))
		   (setq list-old-coor(subst(append punkt-old(list(list "comment" (strcat "����.: name="name-new))))punkt-old list-old-coor))
		   )
		 (setq result(append result(list (strcat "�������|"(convert-ass-list-to-string (nth nn list-old-coor))))))
		 )
	       )
	     (setq n (1- n))
	     (setq nn (+ nn 1000000))
	     )
	    ((equal (list (atof y-new)(atof x-new))
		    (list (atof y-old)(atof x-old))1.5)
	     (if (not year-old)
	       (progn
		 (setq result(append result(list (strcat "������|"(convert-ass-list-to-string punkt-new)))))
		 (setq list-new-coor(vl-remove punkt-new list-new-coor))
		 
		 (setq add-comment
			(if (= name-new name-old)
			  (strcat "����.: N="x-new " E="y-new)
			  (strcat "����.: name="name-new" N="x-new" E="y-new)
			  ))
		 (terpri)
		 (princ add-comment)
		 (if comment-old
		   (setq list-old-coor(subst(subst (list "comment" (strcat comment-old"; "add-comment))(assoc "comment" punkt-old) punkt-old)punkt-old list-old-coor))
		   (setq list-old-coor(subst(append punkt-old(list(list "comment" add-comment)))punkt-old list-old-coor))
		   )
		 (setq result(append result(list (strcat "�������|"(convert-ass-list-to-string (nth nn list-old-coor))))))
		 (setq n (1- n))
		 (setq nn (+ nn 1000000))
		 )))
	    );cond
	  (setq nn (1+ nn))
	  )
	(setq n (1+ n))
;;;	(if result(file_write_list_to_file (strcat(getvar "DWGPREFIX")"raport.txt") result))
	)
      (if result(file_write_list_to_file (strcat(getvar "DWGPREFIX")"raport.txt") result))
      (XML-Remove-Child list-item-old-coor)
      (XML-Remove-Child list-item-new-coor)
      (XML-Save o-doc)
      (setq list-item-old-coor (XML-Add-Child o-doc "item"))
      (XML-Add-Attribute list-item-old-coor "name" "all")
      (setq list-item-new-coor (XML-Add-Child o-doc "item"))
      (XML-Add-Attribute list-item-new-coor "name" "new-coor")
      (setq n 1)
      (foreach x list-old-coor
	(grtext -1 (strcat"������ "(itoa n)"/" (itoa(length list-old-coor))))
	(convert-ass-list-to-punkt-recorg list-item-old-coor x)
	(setq n (1+ n)))
      (XML-Save o-doc)
      (setq n 1)
      (foreach x list-new-coor
	(grtext -1 (strcat"������ "(itoa n)"/" (itoa(length list-new-coor))))
	(convert-ass-list-to-punkt-recorg list-item-new-coor x)
	(setq n (1+ n)))
      (XML-Save o-doc)
      (vlax-release-object list-item-old-coor)
      (vlax-release-object list-item-new-coor)
      (vlax-release-object o-doc)
      )
    (alert "������! �������� ���� ��������� �� ���������!")
    )
  (grtext -1 "")
  (princ)
  )

(defun C:hhh ( / list-all-coor list-item-all-coor list-item-true-coor n o-doc punkt-1 result1 result2)
  (if(setq o-doc(XML-Get-XMLObject (strcat(getvar "DWGPREFIX")"������� ������� ���������.xml")))
    (progn
      (setq list-item-all-coor(XML-Get-Child-ByAttribute o-doc nil "name" "all"))
      (setq list-all-coor (convert-child-to-ass-list list-item-all-coor))
      (setq list-item-true-coor (XML-Add-Child o-doc "item"))
      (XML-Add-Attribute list-item-true-coor "name" "true")
      (setq n 0)
      (while (setq punkt-1 (nth n list-all-coor))
	(grtext -1 (strcat"����������� "(itoa n)"/"(itoa(length list-all-coor))))
	(if(assoc"year"punkt-1)
	  (setq result1(append result1(list punkt-1)))
	  (setq result2(append result2(list punkt-1)))
	  )
	(setq n (1+ n))
	)
      (XML-Remove-Child list-item-all-coor)
      (XML-Save o-doc)
      (setq list-item-all-coor (XML-Add-Child o-doc "item"))
      (XML-Add-Attribute list-item-all-coor "name" "all")
      (setq list-item-true-coor (XML-Add-Child o-doc "item"))
      (XML-Add-Attribute list-item-true-coor "name" "true")
      (setq n 1)
      (foreach x result1
	(grtext -1 (strcat"������ "(itoa n)"/" (itoa(length result1))))
	(convert-ass-list-to-punkt-recorg list-item-true-coor x)
	(setq n (1+ n)))
      (XML-Save o-doc)
      (setq n 1)
      (foreach x result2
	(grtext -1 (strcat"������ "(itoa n)"/" (itoa(length result2))))
	(convert-ass-list-to-punkt-recorg list-item-all-coor x)
	(setq n (1+ n)))
      (XML-Save o-doc)
      (vlax-release-object list-item-true-coor)
      (vlax-release-object list-item-all-coor)
      )
    (alert "������! �������� ���� ��������� �� ���������!")
    )
  (vlax-release-object o-doc)
  (grtext -1 "")
  (princ)
  )
;;;      (wcmatch name-punkt "*@*")
;;;      (setq pos(vl-string-search "-" name-punkt))

;;;(setq tt (car (_vl-times)))
;;;((eval f) l)
;;;(princ (strcat " " (rtos (/ (- (car (_vl-times)) tt) 1000.) 2 3) "sec."))

(defun C:jjj ( / add-comment address-1 address-2 comment-1 comment-2 e-1 e-2
	      list-all-coor list-item-all-coor n n-1 n-2 name-1 name-2 nn o-doc
	      punkt-1 punkt-2 punkt-old result z-1 z-2 princ-string)
  (if(setq o-doc(XML-Get-XMLObject (strcat(getvar "DWGPREFIX")"������� ������� ���������.xml")))
    (progn
      (setq list-item-all-coor(XML-Get-Child-ByAttribute o-doc nil "name" "all"))
      (setq list-all-coor (convert-child-to-ass-list list-item-all-coor))
      (setq n 0)
      (while (setq punkt-1 (nth n list-all-coor))
;;;	(setq tyme-end (car (_vl-times)))
;;;	(if tyme-start (- tyme-start tyme-end))
	(setq princ-string(strcat"����������� "(itoa n)"/"(itoa(length list-all-coor))))
	(grtext -1 princ-string)
	(setq name-1 nil
	      N-1 nil
	      E-1 nil
	      Z-1 nil
	      address-1 nil
	      comment-1 nil)
	(setq name-1 (cadr(assoc"name"punkt-1)))
	(setq N-1 (cadr(assoc"N"punkt-1)))
	(setq E-1 (cadr(assoc"E"punkt-1)))
	(if(cadr(assoc"Z"punkt-1))(setq Z-1 (cadr(assoc"Z"punkt-1))))
	(if(cadr(assoc"address"punkt-1))(setq address-1 (cadr(assoc"address"punkt-1))))
	(if(cadr(assoc"comment"punkt-1))(setq comment-1 (cadr(assoc"comment"punkt-1))))
	(setq nn (1+ n))
;;;	(setq tyme-n 0)
;;;	(setq tyme-start (car (_vl-times)))
	(while (setq punkt-2 (nth nn list-all-coor))
	  (setq name-2 nil
		N-2 nil
		E-2 nil
		Z-2 nil
		address-2 nil
		comment-2 nil
		add-comment nil)
	  (setq name-2 (cadr(assoc"name"punkt-2)))
	  (setq N-2 (cadr(assoc"N"punkt-2)))
	  (setq E-2 (cadr(assoc"E"punkt-2)))
	  (if(cadr(assoc"Z"punkt-2))(setq Z-2 (cadr(assoc"Z"punkt-2))))
	  (if(cadr(assoc"address"punkt-2))(setq address-2 (cadr(assoc"address"punkt-2))))
	  (if(cadr(assoc"comment"punkt-2))(setq comment-2 (cadr(assoc"comment"punkt-2))))
	  (cond
	    ((and(= N-1 N-2)(= E-1 E-2))
	     (setq result(append result(list (strcat "������|"(convert-ass-list-to-string punkt-2)))))
	     (setq list-all-coor(vl-remove punkt-2 list-all-coor))
	     (if (or(/= name-1 name-2)(/= Z-1 Z-2)(/= address-1 address-2))
	       (progn
		 (if comment-1
		   (setq add-comment (strcat comment-1"; ����.:"))
		   (setq add-comment "����.:"))
		 (if(/= name-1 name-2)(setq add-comment(strcat add-comment(strcat" name="name-2))))
		 (if(and Z-2(/= Z-1 Z-2))(setq add-comment(strcat add-comment(strcat" Z="Z-2))))
		 (if(and address-2(/= address-1 address-2))(setq add-comment(strcat add-comment(strcat" address="address-2))))
		 (if(and comment-2(/= comment-1 comment-2))(setq add-comment(strcat add-comment(strcat" comment="comment-2))))
		 (if comment-1
		   (setq list-all-coor(subst(subst (list "comment" add-comment)(assoc "comment" punkt-1) punkt-1)punkt-1 list-all-coor))
		   (setq list-all-coor(subst(append punkt-1(list(list "comment" add-comment)))punkt-1 list-all-coor)))
		 (setq result(append result(list (strcat "�������|"(convert-ass-list-to-string (nth n list-all-coor))))))
		 )
	       )
	     (setq n (1- n))
	     (setq nn (+ nn 1000000))
	     )
	    ((equal (list (atof N-1)(atof E-1))
		    (list (atof N-2)(atof E-2))1.5)
	     (setq result(append result(list (strcat "������|"(convert-ass-list-to-string punkt-2)))))
	     (setq list-all-coor(vl-remove punkt-2 list-all-coor))
	     (if (or(/= name-1 name-2)(/= Z-1 Z-2)(/= address-1 address-2))
	       (progn
		 (if comment-1
		   (setq add-comment (strcat comment-1"; ����.:"))
		   (setq add-comment "����.:"))
		 (if(/= name-1 name-2)(setq add-comment(strcat add-comment(strcat" name="name-2))))
		 (setq add-comment(strcat add-comment(strcat" N="N-2)))
		 (setq add-comment(strcat add-comment(strcat" E="E-2)))
		 (if(and Z-2(/= Z-1 Z-2))(setq add-comment(strcat add-comment(strcat" Z="Z-2))))
		 (if(and address-2(/= address-1 address-2))(setq add-comment(strcat add-comment(strcat" address="address-2))))
		 (if(and comment-2(/= comment-1 comment-2))(setq add-comment(strcat add-comment(strcat" comment="comment-2))))
		 (if comment-1
		   (setq list-all-coor(subst(subst (list "comment" add-comment)(assoc "comment" punkt-1) punkt-1)punkt-1 list-all-coor))
		   (setq list-all-coor(subst(append punkt-1(list(list "comment" add-comment)))punkt-1 list-all-coor)))
		 (setq result(append result(list (strcat "�������|"(convert-ass-list-to-string (nth n list-all-coor))))))
		 )
	       )
	     (setq n (1- n))
	     (setq nn (+ nn 1000000))
	     )
	    )
	  (setq nn (1+ nn))
;;;	  (setq tyme-n (1+ tyme-n))
	  )
	(setq n (1+ n))
	)
      (if result(file_write_list_to_file (strcat(getvar "DWGPREFIX")"raport.txt") result))
      (XML-Remove-Child list-item-all-coor)
      (XML-Save o-doc)
      (setq list-item-all-coor (XML-Add-Child o-doc "item"))
      (XML-Add-Attribute list-item-all-coor "name" "all")
      (setq n 1)
      (foreach x list-all-coor
	(grtext -1 (strcat"������ "(itoa n)"/" (itoa(length list-all-coor))))
	(convert-ass-list-to-punkt-recorg list-item-all-coor x)
	(setq n (1+ n)))
      (XML-Save o-doc)
      (vlax-release-object list-item-all-coor)

      )
    (alert "������! �������� ���� ��������� �� ���������!")
    )
  (vlax-release-object o-doc)
  (grtext -1 "")
  (princ)
  )

(defun C:kkk ( / list-item-old-coor list-old-coor n nn o-doc punkt )
  (if(setq o-doc(XML-Get-XMLObject (strcat(getvar "DWGPREFIX")"������� ������� ���������.xml")))
    (progn
      (setq list-item-old-coor(XML-Get-Child-ByAttribute o-doc nil "name" "all"))
      (setq list-old-coor (convert-child-to-ass-list list-item-old-coor))
      (setq n 0)
      (repeat (length list-old-coor)
	(grtext -1 (strcat"����������� "(itoa n)"/"(itoa(length list-old-coor))))
	(setq punkt (car list-old-coor))
	(setq list-old-coor (cdr list-old-coor))
	(setq nn (analysis-number-item-in-list punkt list-old-coor))
	(if nn(setq result(append result(list (convert-ass-list-to-string (nth nn list-old-coor))))))
	(setq nn nil)
	(setq list-old-coor(append list-old-coor (list punkt)))
	(setq n (1+ n))
	)
      )
    (alert "������! �������� ���� ��������� �� ���������!")
    )
  (if result(file_write_list_to_file (strcat(getvar "DWGPREFIX")"raport.txt") result))
  (vlax-release-object list-item-old-coor)
  (vlax-release-object o-doc)
  (grtext -1 "")
  (princ)
  )

(defun C:fff ( / collection list-item-no-xy-coor n o-doc punkt-recorg result)
  (terpri)
  (setq o-doc(XML-Get-XMLObject (strcat(getvar "DWGPREFIX")"������� ������� ���������_.xml")))
  (setq list-item-no-xy-coor(XML-Get-Child-ByAttribute o-doc nil "name" "no-xy-coor"))
  (setq collection (XML-Get-Children list-item-no-xy-coor nil))
  (setq n 0)
  (while (setq punkt-recorg (vlax-get-property collection 'item n))
    (grtext -1 (strcat(itoa n)"/" (itoa(vlax-get-property collection 'length))))
    (setq result(append result(list (convert-punkt-recorg-to-string punkt-recorg))))
    (setq n (1+ n))
    )
  (file_write_list_to_file (strcat(getvar "DWGPREFIX")"no-xy-coor.txt") result)
  (XML-Save o-doc)
  (vlax-release-object list-item-no-xy-coor)
  (vlax-release-object o-doc)
  (grtext -1 "")
  (princ)
  )

(defun C:zzz ( / list-item-all list-item-new-coor list-new-coor n o-doc)
    ;���������� ��� ���������� �� ������ "��������" � ������
  (if
    (setq o-doc(XML-Get-XMLObject (strcat(getvar "DWGPREFIX")"������� ������� ���������.xml")))
    (progn
      (setq list-item-new-coor(XML-Get-Child-ByAttribute o-doc nil "name" "new-coor"))
      (setq list-item-all(XML-Get-Child-ByAttribute o-doc nil "name" "all"))
      (setq list-new-coor (convert-child-to-ass-list list-item-new-coor))
      (setq n 1)
      (foreach x list-new-coor
	(grtext -1 (strcat"������ "(itoa n)"/" (itoa(length list-new-coor))))
	(convert-ass-list-to-punkt-recorg list-item-all x)
	(setq n (1+ n)))
      (XML-Remove-Child list-item-new-coor)
      (XML-Save o-doc)
      (vlax-release-object list-item-new-coor)
      (vlax-release-object list-item-all)
      ))
  (vlax-release-object o-doc)
  (grtext -1 "")
  (princ)
  )

(defun C:sort-xml ( / LIST-ALL-COOR LIST-ITEM-ALL LIST-ITEM-ALL-COOR N O-DOC SORT-LIST-ALL-COOR)
  (if
    (setq o-doc(XML-Get-XMLObject (strcat(getvar "DWGPREFIX")"������� ���������.xml")))
    (progn
      (setq list-item-all(XML-Get-Child-ByAttribute o-doc nil "name" "no-xy-coor"))
      (setq list-all-coor (convert-child-to-ass-list list-item-all))
      (setq n 1)
      (setq sort-list-all-coor(vl-sort-i list-all-coor(function (lambda (x1 x2)(< (atoi(cadr(assoc"name"x1)))(atoi(cadr(assoc"name"x2))))))))
      (setq list-all-coor (sorting-list-by-num-list list-all-coor sort-list-all-coor))
      (XML-Remove-Child list-item-all)
      (setq list-item-all-coor (XML-Add-Child o-doc "item"))
      (XML-Add-Attribute list-item-all-coor "name" "no-xy-coor")
      (foreach x list-all-coor
	(if (=(rem n 100)0)(grtext -1 (strcat"������ "(itoa n)"/" (itoa(length list-all-coor)))))
	(convert-ass-list-to-punkt-recorg list-item-all-coor x)
	(setq n (1+ n)))
      
      (XML-Save o-doc)
 
      (vlax-release-object list-item-all)
      ))
  (vlax-release-object o-doc)
  (grtext -1 "")
  (princ)
  )

(defun sorting-list-by-num-list (input-list num-list / result n)
  (setq n 1)
  (foreach x num-list
    (if (=(rem n 100)0)(grtext -1 (strcat"����������� "(itoa n)"/" (itoa(length list-all-coor)))))
    (setq result (append result (list (nth x input-list))))
    (setq n (1+ n))
    )
  (grtext -1 "")
  result
  )

(defun draw-mkad ()
  (draw-lwpolyline-vla-by-list-property
    (list(cons "spece" (if (eq (getvar "CTAB")"Model")model_spece paper_spece))
	 (cons "Coordinates" (get-point-list-mkad))
	 (cons "Closed" 1)
	 ))
  )

(defun list-poins-mkad ( / RESULT)
  (foreach x (list-coordinates-lwpolyline(vlax-ename->vla-object(car(entsel))))
  (setq result(append result(list
			      (strcat "'(" (rtos(nth 0 x)2 3) " " (rtos(nth 1 x)2 3) ")")
			      )))
  )
  (file_write_list_to_file (strcat(getvar "DWGPREFIX")"result-list.txt") result)
  (princ)
  )

(defun C:yyy ( / COLLECTION LIST-ITEM-ALL N O-DOC PUNKT RESULT NAME-PUNKT NUMBER)
  (terpri)
  (if
    (setq o-doc(XML-Get-XMLObject (strcat(getvar "DWGPREFIX")"������� ���������.xml")))
    (progn
      (setq list-item-all(XML-Get-Child-ByAttribute o-doc nil "name" "all"))
      (setq collection (XML-Get-Children list-item-all nil))
      (setq n 0)
      (setq number 1000475)
      (while (setq punkt (vlax-get-property collection 'item n))
	(if (=(rem n 100)0)(grtext -1 (strcat(itoa n)"/"(itoa(vlax-get-property collection 'length)))))
	(setq name-punkt(XML-Get-Attribute punkt "name" ""))
	(if(wcmatch name-punkt "*@*")
	  (progn
	    (XML-Put-Attribute punkt "name" (itoa number))
	    (XML-Put-Attribute punkt "comment" (strcat "������������� �: "name-punkt))
	    (setq result(append result(list(strcat name-punkt"=>"(itoa number)))))
	    (setq number (1+ number))
	    )
	  )
;;;	(setq result(append result(list(atoi(XML-Get-Attribute punkt "name" "")))))
	
	(setq n (1+ n))
	)
;;;      (princ  (apply 'max result))
      (XML-Save o-doc)
      (vlax-release-object collection)
      (vlax-release-object list-item-all)
      (file_write_list_to_file (strcat(getvar "DWGPREFIX")"result-ren.txt") result)
      (princ "\n ������!")
      ))
  (vlax-release-object o-doc)
  (grtext -1 "")
  (princ)
  )

(defun C:draw-all ( / COLLECTION LIST-ITEM-ALL N O-DOC PUNKT RESULT NAME-PUNKT NUMBER)
  ;������ ��� ������ �������� �� �������
  (if
    (setq o-doc(XML-Get-XMLObject (strcat(getvar "DWGPREFIX")"������� ���������.xml")))
    (progn
      (setq list-item-all(XML-Get-Child-ByAttribute o-doc nil "name" "all"))
      (setq collection (XML-Get-Children list-item-all nil))
      (setq n 0)
      (while (setq punkt (vlax-get-property collection 'item n))
	(if (=(rem n 100)0)(grtext -1 (strcat(itoa n)"/"(itoa(vlax-get-property collection 'length)))))
	(draw-punkt-by-record punkt)
	(setq n (1+ n))
	)
      (XML-Save o-doc)
      (vlax-release-object collection)
      (vlax-release-object list-item-all)
      (file_write_list_to_file (strcat(getvar "DWGPREFIX")"result-ren.txt") result)
      (princ "\n ������!")
      ))
  (vlax-release-object o-doc)
  (grtext -1 "")
  (princ)
  )

(defun C:find-1 ( / a list-all-coor list-item-all-coor n nn o-doc punkt-1 punkt-2 result tyme-end tyme-n tyme-start)
    ;����� �������������� ������� (������� �� 2��)
  (if(setq o-doc(XML-Get-XMLObject (strcat(getvar "DWGPREFIX")"������� ���������.xml")))
    (progn
      (setq list-item-all-coor(XML-Get-Child-ByAttribute o-doc nil "name" "all"))
      (setq list-all-coor (convert-child-to-ass-list list-item-all-coor))
      (setq n 0)
      (while (setq punkt-1 (nth n list-all-coor))
	(setq a (length list-all-coor))
	(setq tyme-end (car (_vl-times)))
	(if tyme-start
	  (grtext -1 (strcat"����������� "
			    (itoa n)
			    "/"
			    (itoa a)
			    "("
			    (convert-time-sek-to-string(/(*(/(*(1- a)(/(- tyme-end tyme-start)(float tyme-n)))2)a)1000))
			    ")"))
	  (grtext -1(strcat"����������� "(itoa n)"/"(itoa a)))
	  )
	(setq nn (1+ n))
	(setq tyme-n 0)
	(setq tyme-start (car (_vl-times)))
	(while (setq punkt-2 (nth nn list-all-coor))
	  (if (proverka punkt-1 punkt-2)
	  (setq result(append result(list (convert-ass-list-to-string punkt-1)
					  (convert-ass-list-to-string punkt-2)
					  "==============================================================================="
					  ))))
	  (setq nn (1+ nn))
	  (setq tyme-n (1+ tyme-n))
	  )
	(setq n (1+ n))
	)
      (if result(file_write_list_to_file (strcat(getvar "DWGPREFIX")"raport.txt") result))
      (vlax-release-object list-item-all-coor)
      )
    (alert "������! �������� ���� ��������� �� ���������!")
    )
  (vlax-release-object o-doc)
  (grtext -1 "")
  (princ)
  )

(defun proverka (punkt-1 punkt-2 / n1 n2 e1 e2)
  (setq n1 (cadr(assoc"N"punkt-1)))
  (setq e1 (cadr(assoc"E"punkt-1)))
  (setq n2 (cadr(assoc"N"punkt-2)))
  (setq e2 (cadr(assoc"E"punkt-2)))
  (if (and (>(strlen n1)6)(>(strlen n2)6)(>(strlen e1)6)(>(strlen e2)6))
    (if (and(=(substr n1 (-(strlen n1)5))(substr n2 (-(strlen n2)5)))
	    (=(substr e1 (-(strlen e1)5))(substr e2 (-(strlen e2)5))))
      T
      nil)
    nil)
  )

(defun C:find-2 ( /  list-all-coor list-item-all-coor n nn o-doc punkt-1 punkt-2 result flag  )
    ;����� �������������� ������� (������� �� 2��) � �������� ������ ����� ������
  (if(setq o-doc(XML-Get-XMLObject (strcat(getvar "DWGPREFIX")"������� ���������.xml")))
    (progn
      (setq list-item-all-coor(XML-Get-Child-ByAttribute o-doc nil "name" "all"))
      (setq list-all-coor (convert-child-to-ass-list list-item-all-coor))
      (setq n 0)
      (while (setq punkt-1 (nth n list-all-coor))
	(grtext -1(strcat"����������� "(itoa n)"/"(itoa (length list-all-coor))))
	(setq nn (1+ n))
	
	
	(while (if (= (cadr(assoc"name"punkt-1))(cadr(assoc"name"(nth nn list-all-coor))))(setq punkt-2 (nth nn list-all-coor))nil)
	  (if (proverka punkt-1 punkt-2)
	    (progn
	      (draw-line-vla-by-list-property(list
					       (cons "spece" model_spece)
					       (cons "StartPoint" (list(atof(cadr(assoc"E"punkt-1)))(atof(cadr(assoc"N"punkt-1))) 0))
					       (cons "EndPoint" (list(atof(cadr(assoc"E"punkt-2)))(atof(cadr(assoc"N"punkt-2))) 0))
					       (cons "Color" 1)
					       ))
	      (setq result(append result(list (convert-ass-list-to-string punkt-1)
					      (convert-ass-list-to-string punkt-2)
					      "==============================================================================="
					      ))))
	    )
	  (setq nn (1+ nn))
	  
	  (setq flag T)
	  )
	(if flag (setq n nn))
	(setq flag nil)
	(setq n (1+ n))
	)
      (if result(file_write_list_to_file (strcat(getvar "DWGPREFIX")"raport.txt") result))
      (vlax-release-object list-item-all-coor)
      )
    (alert "������! �������� ���� ��������� �� ���������!")
    )
  (vlax-release-object o-doc)
  (grtext -1 "")
  (princ)
  )