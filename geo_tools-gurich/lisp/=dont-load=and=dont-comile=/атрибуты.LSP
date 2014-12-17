(defun c:binc (/ oldStart oldPref oldSuf oldEcho oldInc
        oldSize oldBlock temBl *error* att attr apnum:tag pt )
;;;Новая версия Если в блоке несколько атрибутов, то выбирается какой нужно вставить
;;;http://forum.dwg.ru/showthread.php?t=46382
 
;==== Local functions ============
  (vl-load-com)
  (defun *error* (msg)(setvar "CMDECHO" oldEcho)(setvar "ATTDIA" att)(setvar "ATTREQ" attr)(princ)); end *error*
  (defun mydcl (zagl info-list / fl ret dcl_id)
    (if (null zagl)(setq zagl "Select")) ;_ end of if
    (setq fl (vl-filename-mktemp "mip" nil ".dcl"))
    (setq ret (open fl "w"))
    (mapcar '(lambda (x) (write-line x ret))
            (list "mip_msg : dialog { "
                  (strcat "label=\"" zagl "\";")
                  " :list_box {"
                  "alignment=top ;"
                  "width=51 ;"
                  (if (> (length info-list) 26)
                      "height= 26 ;"
                      (strcat "height= " (itoa (+ 3 (length info-list))) ";")
                  ) ;_ end of if
                  "is_tab_stop = false ;"
                  "key = \"info\";}"
                  "ok_cancel;}"
            ) ;_ end of list
    ) ;_ end of mapcar
    (setq ret (close ret))
    (if (and (not(minusp(setq dcl_id (load_dialog fl))))
	     (new_dialog "mip_msg" dcl_id))
            (progn
                (start_list "info")(mapcar 'add_list info-list)
                (end_list)(set_tile "info" "0")
                (setq ret (car info-list))
                (action_tile "info" "(setq ret (nth (atoi $value) info-list))")
                (action_tile "cancel" "(progn(setq ret nil)(done_dialog 0))")
                (action_tile "accept" "(done_dialog 1)")(start_dialog)
            ) ;_ end of progn
    ) ;_ end of if
    (unload_dialog dcl_id)(vl-file-delete fl) ret) ;_ end of defun
  (defun mip-conv-to-str (dat)
  (cond ((= (type dat) 'INT)(setq dat (itoa dat)))
         ((= (type dat) 'REAL)(setq dat (rtos dat 2 12)))
        ((null dat)(setq dat ""))
        (t (setq dat (vl-princ-to-string dat)))))
;; obj - Ename or Vla object of block
;; att_list - list ((Tag_Name1 . Value1)(Tag_Name2 . Value2) ...)
;;                 Tag_Name - string
;;                    Value - string
(defun mip-block-setattr-bylist (obj att_list / txt lst)
(if (= (type obj) 'ENAME)(setq obj (vlax-ename->vla-object obj)))
(setq att_list (mapcar '(lambda(x)(cons (strcase (mip-conv-to-str(car x)))(mip-conv-to-str(cdr x)))) att_list))
  (if (and obj
           (not(vlax-erased-p obj))
           (= (vla-get-ObjectName obj) "AcDbBlockReference")
	   (eq :vlax-true (vla-get-HasAttributes obj))
	   (vlax-property-available-p obj 'Hasattributes)
	   (vlax-write-enabled-p obj)
      )
    (vl-catch-all-apply
      (function
	(lambda	()
          (foreach at (vlax-invoke obj 'Getattributes)
            (if (setq lst (assoc(strcase(vla-get-TagString at)) att_list))
              (vla-put-TextString at (cdr lst))
            )
            )
          )
        )
      )
    )
  )
  
  (defun get-all-atts (obj)
    ;;;Use (get-all-atts (car(entsel "\nSelect block:")))
    ;;;Returs list  (("TAG1" . "Value1")("TAG2" . "Value2") ...)
    (if (= (type obj) 'ENAME)
		(setq obj (vlax-ename->vla-object obj)))
  (if (and obj
           (vlax-property-available-p obj 'Hasattributes)
	   (eq :vlax-true (vla-get-HasAttributes obj))
      )
    (vl-catch-all-apply
      (function
	(lambda	()
	  (mapcar (function (lambda (x)
			      (cons (vla-get-TagString x)
				    (vla-get-TextString x)
			      )
			    )
		  )
		  (append (vlax-invoke obj 'Getattributes)
			  (vlax-invoke obj 'Getconstantattributes)
		  )
	  )
	)
      )
    )
  )
)
(defun rec-pat (str temp)
  (cond
    ((= str "")(if (/= temp "")(list temp)))
    ((wcmatch (substr str 1 1) "[1234567890.]")
     (rec-pat (substr str 2) (strcat temp (substr str 1 1)))
    )
    (t (if (/= temp "")
        (cons temp (rec-pat str ""))
        (rec-pat (substr str 2) "")
       ) ;_ end of if
    )
  ) ;_ end of cond
) ;_ end of defun

  
;==== Local functions END ============
  
  (if(not apnum:Size)(setq apnum:Size 1.0))
  (if(not apnum:Num)(setq apnum:Num 1))
  (if(not apnum:Inc)(setq apnum:Inc 1))
  (if(not apnum:Pref)(setq apnum:Pref ""))
  (if(not apnum:Suf)(setq apnum:Suf ""))
  (setq  oldStart apnum:Num oldSize apnum:Size oldInc apnum:Inc
         oldPref apnum:Pref oldSuf apnum:Suf
         apnum:Block (mip-conv-to-str apnum:Block)
         apnum:tag (mip-conv-to-str apnum:tag)
         oldEcho (getvar "CMDECHO")
         att (getvar "ATTDIA") attr (getvar "ATTREQ")); end setq
  (setvar "ATTDIA" 0)(setvar "ATTREQ" 0)
  (setvar "CMDECHO" 0)
    (setq apnum:Pref
    (getstring T
      (strcat "\nType prefix:(If you want to delete prefix type ' ' (press SPACE key on keyboard) <"apnum:Pref"> :")))
  (if(= "" apnum:Pref)(setq apnum:Pref oldPref))
  (if(= " " apnum:Pref)(setq apnum:Pref ""))
  (setq apnum:Suf
    (getstring T
       (strcat "\nType suffix:(If you want to delete prefix type ' ' (press SPACE key on keyboard) <"apnum:Suf"> :")))
  (if(= "" apnum:Suf)(setq apnum:Suf oldSuf))
  (if(= " " apnum:Suf)(setq apnum:Suf ""))
  (if apnum:Block(setq oldBlock apnum:Block))
  (setq temBl
     (entsel(strcat "\nSelect block <"
         (if apnum:Block apnum:Block "not difined") "> > "))); end setq
  (cond
    ((or (and tembl (= 1 (cdr(assoc 66(entget(car temBl)))))
	  (= "INSERT" (cdr(assoc 0(entget(car temBl)))))
	  )
	 (and apnum:Block (setq tembl (tblobjname "BLOCK" apnum:Block))
	      (setq tembl (list tembl))
	      )
	 )
      (setq apnum:Block(cdr(assoc 2(entget(car temBl)))))
      ((lambda( / lst e1 ed ss i)
        (setq e1 (entnext (car temBl)))
        (while (AND e1
		  (wcmatch (cdr (assoc 0 (setq ed (entget e1)))) "ATTRIB,ATTDEF")
	     ) ;_ End of AND
	  (setq lst (cons (cdr (assoc 2 ed)) lst))
	  (setq e1 (entnext e1))
	)
	 (cond ((= 0 (length lst))(setq apnum:Block nil))
	       ((= 1 (length lst))(setq apnum:tag (car lst)))
	       (t (setq apnum:tag (mydcl "Select attribute" (acad_strlsort lst))))
	       )
;;; Максимальный номер берется как максимальное число всех значений атрибутов
;;; Высчитыватся так (допустим значение атрибута   D3SE/0-008A18B3:
;;;        Значение префикса D3SE/0-
;;;        Значение суффикса B3 
;;;         1. Из значения атрибута удаляется префикс и суффикс
;;;                Значение атрибута - 008A18
;;;         2. В значении атрибута выбираются все цифры ("008" "18")
;;;         3. Из них за число атрибута берется число с максимальной последовательностью цифр
;;;                    т.е. 8, а не 18
	 
	 (and
	   (setq ss nil ss (ssget "_X" (list
				'(0 . "INSERT")
				(cons 2  apnum:Block)
				(cons 410 (getvar "CTAB")))))
	   (setq lst nil i '-1)
	   (progn
	     (repeat (sslength ss)
	        (setq lst (cons (ssname ss (setq i (1+ i))) lst))
	       )
	     lst
	     )
         (setq apnum:Num (1+
	      (apply
		'max
	        (mapcar 'atoi
                (mapcar 'mip-conv-to-str        
	        (mapcar '(lambda(f / mst)
			   (setq mst (apply 'max (mapcar 'strlen f)))
			   (car(vl-remove-if-not '(lambda(f1)(= mst (strlen f1))) f))
			   )
	        (mapcar '(lambda(z)(rec-pat z ""))
		(mapcar '(lambda(y)
	         (vl-string-right-trim (mip-conv-to-str apnum:Suf)
		 (vl-string-left-trim apnum:pref y)))
		 (vl-remove-if 'null (mapcar '(lambda(x)(cdr(assoc (strcase apnum:tag)(get-all-atts x)))) lst)))
		)))))))
	   );_and
	 );_lambda
	);_lambda
    ); end condition #2
    ((null (tblsearch "BLOCK" apnum:Block))
     (alert (strcat "Block " apnum:Block " not found"))
     (setq apnum:Block nil)
     ); end condition #0
    ((and apnum:Block(not temBl)(tblsearch "BLOCK" apnum:Block))
    (setq apnum:Block oldBlock)
     ); end condition #1
    
    (t
     (princ "\nBlock not contains attribute! ")
     (setq apnum:Block nil)
     ); end condition #3
    ); end cond  
  (setq apnum:Num
    (getint
      (strcat "\nSpecify start number <"(itoa apnum:Num)">: ")))
  (if(null apnum:Num)(setq apnum:Num oldStart))
  (setq apnum:Inc
    (getint
      (strcat "\nSpecify increment <"(itoa apnum:Inc)">: ")))
  (if(null apnum:Inc)(setq apnum:Inc oldInc))
  (setq apnum:Size
    (getreal
      (strcat "\nSpecify block scale <"(rtos apnum:Size)">: ")))
  (if(null apnum:Size)(setq apnum:Size oldSize))

  (if (and apnum:Block apnum:tag)
    (progn
(while T
  (princ "\n>>> Pick insertion point or press Esc to quit <<<\n")
  (command "_-insert" apnum:Block "_s" apnum:Size pause "0")
   (mip-block-setattr-bylist (entlast)
     (list(cons (strcase (mip-conv-to-str apnum:tag))
                (strcat apnum:Pref(itoa apnum:Num)apnum:Suf))))
    (setq apnum:Num (+ apnum:Num apnum:Inc))
  ); end while
); end progn
    ); end if
  (setvar "ATTDIA" att)(setvar "ATTREQ" attr)
  (princ)
  )