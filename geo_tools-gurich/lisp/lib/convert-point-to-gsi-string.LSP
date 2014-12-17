(defun convert-point-to-gsi-string (input-list / description e n number number-item z)
  (setq number-item(cdr(assoc "number-item" input-list)))
  (setq number(cdr(assoc "number" input-list)))
  (setq N(cdr(assoc "N" input-list)))
  (setq E(cdr(assoc "E" input-list)))
  (setq Z(cdr(assoc "Z" input-list)))
  (setq description(cdr(assoc "description" input-list)))
  (if number-item
    (progn
      (strcat
	"*11"
	(edit-string-add-symbol (itoa number-item) "0" 4 T)
	"+"
	(edit-string-add-symbol (if(=(type number)'STR)number(itoa number))"0" 16 T)
	" 71....+"
	(edit-string-add-symbol description "0" 16 T)
	" 81..10+"
	(edit-string-add-symbol (rtos(* N 1000)2 0)  "0" 16 T)
	" 82..10+"
	(edit-string-add-symbol (rtos(* E 1000)2 0)  "0" 16 T)
	" 83..10+"
	(edit-string-add-symbol (rtos(* Z 1000)2 0)  "0" 16 T)
	" "
	)
      )
    nil
    )
  )

(defun edit-string-add-symbol (text symbol kol_sym flag / ); flag - T добавлять в начало nil в конец
  (repeat (- kol_sym (strlen text))
    (if flag
      (setq text (strcat symbol text))
      (setq text (strcat text symbol))
      )
    )
  text
  )
;;;"*110001+0000000000000001 71....+00000000000first 81..10+0000000000100000 82..10+0000000000100000 83..10+0000000000100000 "
;;;(convert-point-to-gsi-string(list (cons "number-item" 1)(cons "number" 1) (cons "N" 100.00) (cons "E" 100.00) (cons "Z" 100.00) (cons "code" "code")))