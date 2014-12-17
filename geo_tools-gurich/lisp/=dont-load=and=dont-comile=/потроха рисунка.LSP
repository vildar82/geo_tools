;;;автор Эдуард Смолянка #103-09-2007 16:33:17 
;;;Иногда бывает полезно заглянуть в потроха рисунка 
;;;Программка до ужаса простая. Тупо перебирает метки
;;;переводя их из шестнадцатеричной системы исчисления в десятичную и обратно.


;;;Функция преобразования числа из шестнадцатиричной в десятичную систему исчисления
;;;аргументы :
;;;а - число в шестнадцатиричной системе исчисления , тип string
;;;переменные:
;;;listst - список степеней числа 16. Длина списка равна количеству символов аргумента
(defun H-dec (a / i listst)
  (setq i (strlen a))
  (repeat i
    (setq listst (cons (expt 16 (1- i)) listst))
    (setq i (1- i))
    )
  (rtos
    (apply
      '+
      (mapcar
	'*
	(reverse
	  (mapcar
	    '(lambda (x)
	       (cond
		 (
		  (vl-symbolp (read (chr x)))
		  (+ (vl-position (chr x) '("A" "B" "C" "D" "E" "F"))
		     10
		     )
		  )
		 (t
		  (atoi (chr x))
		  )
		 )
	       )
	    (vl-string->list a)
	    )
	  )
	(mapcar 'float listst)
	)
      )
    )
  )
;;;Функция преобразования числа из десятичной в шестнадцатиричную систему исчисления.
;;;Аргументы:
;;;b- число в десятичной системе исчисления , тип integer
;;;Переменные:
;;;blist - число в шестнадцатеричной системе исчисления , тип string
(defun Dec-H (b / blist list16)
  (setq   list16 '("0" "1" "2" "3" "4" "5" "6" "7" "8" "9" "A" "B" "C" "D"
		   "E" "F")
	  )
  (while (>= b 16)
    (setq blist   (cons (fix (rem b 16)) blist)
	  b   (fix (/ b 16.00))
	  )
    )
  (apply 'strcat
	 (mapcar '(lambda (x)
		    (nth x list16)
		    )
		 (setq blist (cons b blist))
		 )
	 )
  )
;;; Функция экспорта базы данных рисунка в текстовой файл. Производит поиск графических и не графических примитивов в базе рисунка
;;;перебирая метки с 1 до метки последнего созданного примитива , выводит свойства примитивов в текстовой файл
;;; в виде :  "класс объекта"  "метка"  "метка в десятичном представлении" "имя объекта" (при его наличии)
;;; аргументов нет
;;; переменные :
;;;filename - имя файла вывода данных
;;;dsk - дескриптор файла
;;;lp - имя последнего созданного примитива
;;;m - метка примитива
;;;lm - список меток
(defun es_export_database (/ lm filename dsk lp m)
  (setq i 1)
  (if (and
	(setq filename (getfiled "Файл вывода " "C:\\" "xls" 1))
	;имя файла вывода
	(setq dsk (open filename "a"))   ;дескриптор файла
	(setq lp (entmakex '((0 . "point") (10 0 0 0))))
	; создаем примитив для определения количества повторов цикла перебора меток
	)
    (progn
      (repeat (1- (atoi (h-dec (cdr (assoc 5 (entget lp))))))
	;цикл перебора меток
	(if
	  (setq m (handent (Dec-H i)))
	  (setq lm (cons m lm))   ; список содержащий метки существующих в чертеже примитивов
	  )
	(setq i   (1+ i)
	      )
	)
      (entdel lp)         ; удаляем рабочий примитив
      (mapcar
	'(lambda (x / nam prn)
	   (setq prn (strcat (vla-get-objectname x)
			     "\t"
			     (vla-get-handle x)
			     "\t"
			     (h-dec (vla-get-handle x))
			     "\t"
			     (rtos (vla-get-objectid x) 2 0)
			     "\t"
			     )
		 )
	   (if
	     (and
	       (vlax-property-available-p x 'Name)
	       (not (vl-catch-all-error-p
		      (setq
			nam (vl-catch-all-apply 'vla-get-name (list x))
			)
		      )
		    )
	       )
	     (setq prn (strcat prn nam "\t"))
	     (setq prn (strcat prn "\t"))
	     )
	   (princ (strcat prn "\n") dsk)
	   )
	(vl-remove 'nil
	  (mapcar 'vlax-ename->vla-object (reverse lm))
	  )
	)               ; функция печати данных примитива в файл
      (close dsk)
      )
    )
  )