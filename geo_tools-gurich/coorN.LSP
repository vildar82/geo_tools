;;;;        Design by  : VVA <Vladimir Azarko>
;;;;        Posted     :

;|
Экспорт координат указанных точек, выбранных объектов: точек, блоков, полилиний, сплайнов в текстовый файл, ексел с простановкой номеров
Текстовый файл — либо txt, либо csv.
Округление координат в соответствии с текущими настройками команды _UNITS (переменная LUPREC !!!)
!!!!!!!!!!!!!
Набрать в командной строке LUPREC и установить нужную точность округления.
!!!!!!!!!!

Определены 3 команды
COOR - экспорт координат
COORN -экспорт координат с нумерацией
COORT -экспорт координат с нумерацией, где номером считается ближайший к точке текст
|;

;;http://www.caduser.ru/cgi-bin/f1/board.cgi?t=39175jU
;|=============== Команда COORN ===============================================

EN:
   Export of coordinates of the specified points, the chosen objects: points, blocks, polylines, splines in a text file, Excel.
   Text file — txt, or csv. A rounding off of coordinates according to current adjustments of a command _UNITS (LUPREC !!!)
RUS:
Экспорт координат указанных точек, выбранных объектов: точек, блоков, полилиний, сплайнов в текстовый файл, ексел с простановкой номеров
Текстовый файл — либо txt, либо csv.
Номера точек отрисовываются текстом на текущем слое, текущим стилем, текущей высотой
Округление координат в соответствии с текущими настройками команды _UNITS (переменная LUPREC !!!)

|;

(defun c:COORN (/ cFile curPt filPath objSet oFlag oldMode ptLst sFlag lw isRus Npt)
(defun group-by-num (lst num / ls ret)(if (= (rem (length lst) num ) 0)(progn (setq ls nil)
  (repeat (/ (length lst) num)(repeat num (setq ls(cons (car lst) ls)lst (cdr lst)))
  (setq ret (append ret (list (reverse ls))) ls nil)))) ret)
(defun PtCollect(SelSet)(mapcar 'cdr (mapcar '(lambda(x)(assoc 10 x))(mapcar 'entget
(vl-remove-if 'listp(mapcar 'cadr(ssnamex SelSet))))))); end of PtCollect
(defun PLCollect(SelSet / ret)
(foreach lw (mapcar 'vlax-ename->vla-object (vl-remove-if 'listp(mapcar 'cadr(ssnamex SelSet))))
  (cond ((wcmatch (vla-get-ObjectName lw) "*Polyline")
  (setq ret (append ret (group-by-num (vlax-get lw 'Coordinates)
  (if (=(vla-get-ObjectName lw) "AcDbPolyline") 2 3)))))
       ((=(vla-get-ObjectName lw) "AcDbSpline")(setq ret (append ret (group-by-num
         (vlax-safearray->list(vlax-variant-value (vla-get-controlpoints lw)))  3))))
      (t nil))) ret)
  (vl-load-com)(setq isRus(= (getvar "SysCodePage") "ANSI_1251"))(if(not ptcol:mode)(setq ptcol:mode "Pick"))
  (initget "Указать Точка Блоки Полилиния Pick pOints Blocks poLyline _Pick pOints Blocks poLyline Pick pOints Blocks poLyline")
(setq oldMode ptcol:mode ptcol:mode
(getkword (if IsRus (strcat "\nВыберите режим [Указать/Точка/Блоки/Полилиния или сплайн] <"
(cadr (assoc ptcol:mode '(("Pick" "Указать")("pOints" "Указать")("Blocks" "Блоки")("poLyline" "Полилиния")))) ">: ")
        (strcat "\nSpecify mode [Pick/pOints/Blocks/poLyline or spline] <"ptcol:mode">: "))) ptLst nil)
(if(null ptcol:mode)(setq ptcol:mode oldMode))
(cond ((= "Pick" ptcol:mode)(setq curPt T)
       (while curPt (setq curPt(getpoint (if IsRus
         "\nУкажите точку или Enter завершения > " "\nPick point or Enter to continue > ")))
  (if curPt (setq ptLst(append ptLst(list (trans curPt 1 0))))))); end condition #1
      ((= "pOints" ptcol:mode)(if (not(setq objSet(ssget "_I" '((0 . "POINT")))))(progn
         (if IsRus (princ "\nВыберите точки и нажмите Enter ")(princ "\nSelect points and press Enter "))
   (setq objSet(ssget '((0 . "POINT"))))))(if objSet (setq ptLst(PtCollect objSet)))); end condition #2
      ((= "Blocks" ptcol:mode)(if (not(setq objSet(ssget "_I" '((0 . "INSERT")))))(progn
        (if IsRus(princ "\nВыберите блоки и нажмите Enter ")(princ "\nSelect blocks and press Enter "))
     (setq objSet(ssget '((0 . "INSERT"))))))(if objSet (setq ptLst(PtCollect objSet)))); end condition #3
      ((= "poLyline" ptcol:mode)(if (not(setq objSet(ssget "_I" '((0 . "*POLYLINE,SPLINE")))))(progn
        (if IsRus(princ "\nВыберите полилинии и нажмите Enter  ")(princ "\nSelect polyline and press Enter "))
     (setq objSet(ssget '((0 . "*POLYLINE,SPLINE"))))))(if objSet (setq ptLst(PLCollect objSet)))); end condition #4
); end cond
(if ptLst (progn (princ "\n+++++++ Coordinates list +++++++\n")(setq ptLst (mapcar '(lambda(x)(trans x 0 1)) ptLst))
(mapcar '(lambda(x)(princ(strcat "\n"(rtos(car x))","(rtos(cadr x))
(if(= 3(length x))(strcat ","(rtos(nth 2 x))) "")))) ptLst); end mapcar
(princ "\n\n+++++++++ End of list +++++++++")
(setq Npt (getint (if IsRus "\nНачальный номер точки <Не маркировать> : " "\nStart number of points <Don't mark> : " )))
(initget "Файл Excel Не Text Excel Not _Text Excel Not Text Excel Not")
(setq sFlag (getkword (if IsRus "\nСохранить координаты в [Файл/Excel/Не сохранять] <Файл> : "
"\nSave coordinates to [Text file/Excel/Not save] <Text> : ")))
(if(null sFlag)(setq sFlag "Text"))(setq oFlag Npt)(if (numberp Npt)
(foreach ln ptlst
  (text-draw                 ;_Отрисовка текста
    (itoa Npt)               ;_Номер точки
    (polar ln (/ pi 4) 1.)   ;_Координаты на 1 ед по углом 45 градусов
    (getvar "TEXTSIZE")      ;_ Текущей высотой текста
    0                        ;_Угол поворота
    nil
    )
  (setq Npt (1+ Npt))))
(setq Npt oFlag)    
(setq ptLst (mapcar '(lambda(x)(mapcar 'rtos x)) ptlst))
(cond ((and (= "Text" sFlag)(setq filPath
       (getfiled (if IsRus "Сохранение координат в текстовый файл" "Save Coordinates to Text File") "Coordinates.txt" "txt;csv" 33)))
       (setq cFile(open filPath "w"))(foreach ln ptLst (write-line (strcat (if (numberp Npt)(strcat (itoa Npt) ",") "")(car ln)","(cadr ln)
         (if(= 3(length ln))(strcat ","(nth 2 ln)))) cFile)(if (numberp Npt)(setq Npt (1+ Npt))))(close cFile)(initget "Yes No")
       (setq oFlag(getkword (if IsRus "\nОткрыть файл? [Yes/No] <No> : " "\nOpen text file? [Yes/No] <No> : " )))
       (if(= oFlag "Yes")(startapp "notepad.exe" filPath))); end condition #1
     ((= "Excel" sFlag)(if (numberp Npt)(progn
      (setq ptlst (mapcar '(lambda(x)(cons (1- (setq Npt (1+ Npt))) x)) ptlst))
      (xls ptlst '("N" "X" "Y" "Z") nil "COORN"))
      (xls ptLst nil nil "COOR"))); end condition #2
     (t nil)))) (princ)); end of c:COOR
;|================== XLS ========================================
*  published http://www.autocad.ru/cgi-bin/f1/board.cgi?t=31371zf
               http://www.autocad.ru/cgi-bin/f1/board.cgi?t=31596eW
* Purpose: Export of the list of data Data-list in Excell
*             It is exported to a new leaf of the current book.
              If the book is not present, it is created
* Arguments:
              Data-list — The list of lists of data (LIST)
                            ((Value1 Value2 ... VlalueN)(Value1 Value2 ... VlalueN)...)
                            Each list of a kind (Value1 Value2... VlalueN) enters the name in
                            a separate line in corresponding columns (Value1-A Value2-B and .т.д.)
                  header —  The list (LIST) headings or nil a kind (" Signature A " " Signature B "...)
                            If header nil, is accepted ("X" "Y" "Z")
                 Colhide —  The list of alphabetic names of columns to hide or nil — to not hide ("A" "C" "D") — to hide columns A, C, D
                 Name_list — The name of a new leaf of the active book or nil — is not present
* Return: nil
* Usage
(xls '((1.1 1.2 1.3 1.4)(2.1 2.2 2.3 2.4)(3.1 3.2 3.3 3.4)) '("Col1" "Col2" "Col3"  "Col4") '("B") "test")   |;


;|================== XLS ========================================
* Опубликовано http://www.autocad.ru/cgi-bin/f1/board.cgi?t=19833nl&page=2
               http://www.autocad.ru/cgi-bin/f1/board.cgi?t=31371zf
               http://www.autocad.ru/cgi-bin/f1/board.cgi?t=31596eW
* Автор: Владимир Азарко aka VVA
* Назначение: Печать списка данных Data-list в Excell
*             Для вывода создается новая книга
              Вывод осуществляется в первом листе
* Аргументы:
              Data-list — список списков данных (LIST) вида
                            ((Value1 Value2 ... VlalueN)(Value1 Value2 ... VlalueN)...)
                            Каждый список вида (Value1 Value2 ... VlalueN) записывается
                            в отдельную строку в соответствующие столбцы (Value1-A Value2-B и .т.д.)
                  header —  список (LIST) заголовков или nil вида ("Подпись A" "Подпись B" ...)
                            Если header nil, принимается ("X" "Y" "Z")
                 Colhide —  список буквенных названий стоблцов для скрытия или nil — не скрывать
                            ("A" "C" "D") — скрыть столбцы A, C, D
                 Name_list — имя нового листа активной книги или nil — новая книга
* Возврат: nil
* TIPS!!! : При передачи функции xls числовых вещественных данных нет необходимости проверять текущий системный
            разделитель целой и дробной части ("HKEY_CURRENT_USER\\Control Panel\\International" "sDecimal")
            Функцией на время вывода отключается использование в Excele системного разделителя, разделителем
            целой и дробной части устанавливается точка. После завершения ф-ции все восстанавливается.
Пример вызова
(xls '((1.1 1.2 1.3 1.4)(2.1 2.2 2.3 2.4)(3.1 3.2 3.3 3.4)) '("Столбец1" "Столбец2" "Столбец3" "Столбец4") '("B"))|;
(vl-load-com)
(defun xls ( Data-list header Colhide Name_list / *aplexcel* *books-colection* Currsep
*excell-cells* *new-book* *sheet#1* *sheet-collection* col iz_listo row cell cols)
(defun Letter (N / Res TMP)(setq Res "")(while (> N 0)(setq TMP (rem N 26)
  TMP (if (zerop TMP)(setq N (1- N) TMP 26) TMP)
  Res (strcat (chr (+ 64 TMP)) Res)  N   (/ N 26))) Res)
(if (null Name_list)(setq Name_list ""))
  (setq  *AplExcel*     (vlax-get-or-create-object "Excel.Application"))
  (if (setq *New-Book*  (vlax-get-property *AplExcel* "ActiveWorkbook"))
    (setq *Books-Colection*  (vlax-get-property *AplExcel* "Workbooks")
          *Sheet-Collection* (vlax-get-property *New-Book* "Sheets")
               *Sheet#1*     (vlax-invoke-method *Sheet-Collection* "Add"))
(setq *Books-Colection*  (vlax-get-property *AplExcel* "Workbooks")
              *New-Book*     (vlax-invoke-method *Books-Colection* "Add")
          *Sheet-Collection* (vlax-get-property *New-Book* "Sheets")
               *Sheet#1*     (vlax-get-property *Sheet-Collection* "Item" 1)))
(setq *excell-cells*     (vlax-get-property *Sheet#1* "Cells"))
(setq Name_list (if (= Name_list "")
                  (vl-filename-base(getvar "DWGNAME"))
                  (strcat (vl-filename-base(getvar "DWGNAME")) "&" Name_list))
   col 0 cols nil)
(if (> (strlen Name_list) 26)
(setq Name_list (strcat (substr Name_list 1 10) "..." (substr Name_list (- (strlen Name_list) 13) 14))))
(vlax-for sh *Sheet-Collection* (setq cols (cons (strcase(vlax-get-property sh 'Name)) cols)))
(setq row Name_list)
(while (member (strcase row) cols)(setq row (strcat Name_list " (" (itoa(setq col (1+ col)))")")))
(setq Name_list row)
(vlax-put-property *Sheet#1* 'Name Name_list)
(setq Currsep (vlax-get-property *AplExcel* "UseSystemSeparators"))
(vlax-put-property *AplExcel* "UseSystemSeparators" :vlax-false) ;_не использовать системные установки
(vlax-put-property *AplExcel* "DecimalSeparator" ".")            ;_разделитель дробной и целой части
(vlax-put-property *AplExcel* "ThousandsSeparator" " ")          ;_разделитель тысячей
(vla-put-visible *AplExcel* :vlax-true)(setq row 1 col 1)
(if (null header)(setq header '("X" "Y" "Z")))
(repeat (length header)(vlax-put-property *excell-cells* "Item" row col
(vl-princ-to-string (nth (1- col) header)))(setq col (1+ col)))(setq  row 2 col 1)
(repeat (length Data-list)(setq iz_listo (car Data-list))(repeat (length iz_listo)
(vlax-put-property *excell-cells* "Item" row col (vl-princ-to-string (car iz_listo)))
(setq iz_listo (cdr iz_listo) col (1+ col)))(setq Data-list (cdr Data-list))(setq col 1 row (1+ row)))
(setq col (1+(length header)) row (1+ row))
(setq cell (vlax-variant-value (vlax-invoke-method *Sheet#1* "Evaluate"
    (strcat "A1:" (letter col)(itoa row))))) ;_ end of setq
(setq cols (vlax-get-property cell  'Columns))
(vlax-invoke-method cols 'Autofit)
(vlax-release-object cols)(vlax-release-object cell)
(foreach item ColHide (if (numberp item)(setq item (letter item)))
(setq cell (vlax-variant-value (vlax-invoke-method *Sheet#1* "Evaluate"
    (strcat item "1:" item "1"))))
(setq cols (vlax-get-property cell  'Columns))
(vlax-put-property cols 'hidden 1)
(vlax-release-object cols)(vlax-release-object cell))
(vlax-put-property *AplExcel* "UseSystemSeparators" Currsep)
(mapcar 'vlax-release-object (list *excell-cells* *Sheet#1* *Sheet-Collection* *New-Book* *Books-Colection*
*AplExcel*))(setq *AplExcel* nil)(gc)(gc)(princ))
;;;Отрисовка текста
;;; txt - текст
;;; pnt - точка отрисовки в ПСК
;;; heigtht - высота
;;; rotation - угол поворота
;;;justification - или nil
;;;Возвращает имя примитива
(defun text-draw (txt pnt height rotation justification)
   (if (null pnt)(command "_.-TEXT" "" txt)
   (if (= (cdr (assoc 40 (tblsearch "STYLE" (getvar "TEXTSTYLE"))))
    0.0
       ) ;_ end of =
     (progn
     ;; нулевая высота текста
       (if justification
   (command "_.-TEXT" "_J" justification "_none" pnt height rotation txt)
   (command "_.-TEXT" "_none" pnt height rotation txt)
       ) ;_ end of if
     ) ;_ end of progn
     (progn
       ;; фиксированнная высота
       (if justification
   (command "_.-TEXT" "_J" justification "_none" pnt rotation txt)
   (command "_.-TEXT" "_none" pnt rotation txt)
       ) ;_ end of if
     ) ;_ end of progn
   ) ;_ end of if
     )
  (entlast)
)
(defun c:COOR(/ cFile curPt filPath objSet oFlag oldMode ptLst sFlag lw isRus)
(defun group-by-num (lst num / ls ret)(if (= (rem (length lst) num ) 0)(progn (setq ls nil)
  (repeat (/ (length lst) num)(repeat num (setq ls(cons (car lst) ls)lst (cdr lst)))
  (setq ret (append ret (list (reverse ls))) ls nil)))) ret)
(defun PtCollect(SelSet)(mapcar 'cdr (mapcar '(lambda(x)(assoc 10 x))(mapcar 'entget
(vl-remove-if 'listp(mapcar 'cadr(ssnamex SelSet))))))); end of PtCollect
(defun PLCollect(SelSet / ret)
(foreach lw (mapcar 'vlax-ename->vla-object (vl-remove-if 'listp(mapcar 'cadr(ssnamex SelSet))))
  (cond ((wcmatch (vla-get-ObjectName lw) "*Polyline")
	(setq ret (append ret (group-by-num (vlax-get lw 'Coordinates)
	(if (=(vla-get-ObjectName lw) "AcDbPolyline") 2 3)))))
       ((=(vla-get-ObjectName lw) "AcDbSpline")(setq ret (append ret (group-by-num
         (vlax-safearray->list(vlax-variant-value (vla-get-controlpoints lw)))  3))))
      (t nil))) ret)
  (vl-load-com)(setq isRus(= (getvar "SysCodePage") "ANSI_1251"))(if(not ptcol:mode)(setq ptcol:mode "Pick"))
  (initget "Указать Точка Блоки Полилиния Pick pOints Blocks poLyline _Pick pOints Blocks poLyline Pick pOints Blocks poLyline")
(setq oldMode ptcol:mode ptcol:mode
(getkword (if IsRus (strcat "\nВыберите режим [Указать/Точка/Блоки/Полилиния или сплайн] <"
(cadr (assoc ptcol:mode '(("Pick" "Указать")("pOints" "Указать")("Blocks" "Блоки")("poLyline" "Полилиния")))) ">: ")
	      (strcat "\nSpecify mode [Pick/pOints/Blocks/poLyline or spline] <"ptcol:mode">: "))) ptLst nil)
(if(null ptcol:mode)(setq ptcol:mode oldMode))
(cond ((= "Pick" ptcol:mode)(setq curPt T)
       (while curPt (setq curPt(getpoint (if IsRus
         "\nУкажите точку или Enter завершения > " "\nPick point or Enter to continue > ")))
	(if curPt (setq ptLst(append ptLst(list (trans curPt 1 0))))))); end condition #1
      ((= "pOints" ptcol:mode)(if (not(setq objSet(ssget "_I" '((0 . "POINT")))))(progn
         (if IsRus (princ "\nВыберите точки и нажмите Enter ")(princ "\nSelect points and press Enter "))
	 (setq objSet(ssget '((0 . "POINT"))))))(if objSet (setq ptLst(PtCollect objSet)))); end condition #2
      ((= "Blocks" ptcol:mode)(if (not(setq objSet(ssget "_I" '((0 . "INSERT")))))(progn
        (if IsRus(princ "\nВыберите блоки и нажмите Enter ")(princ "\nSelect blocks and press Enter "))
	   (setq objSet(ssget '((0 . "INSERT"))))))(if objSet (setq ptLst(PtCollect objSet)))); end condition #3
      ((= "poLyline" ptcol:mode)(if (not(setq objSet(ssget "_I" '((0 . "*POLYLINE,SPLINE")))))(progn
        (if IsRus(princ "\nВыберите полилинии и нажмите Enter  ")(princ "\nSelect polyline and press Enter "))
	   (setq objSet(ssget '((0 . "*POLYLINE,SPLINE"))))))(if objSet (setq ptLst(PLCollect objSet)))); end condition #4
); end cond
(if ptLst (progn (princ "\n+++++++ Coordinates list +++++++\n")(setq ptLst (mapcar '(lambda(x)(trans x 0 1)) ptLst))
(mapcar '(lambda(x)(princ(strcat "\n"(rtos(car x))","(rtos(cadr x))
(if(= 3(length x))(strcat ","(rtos(nth 2 x))) "")))) ptLst); end mapcar
(princ "\n\n+++++++++ End of list +++++++++")(initget "Файл Excel Не Text Excel Not _Text Excel Not Text Excel Not")
(setq sFlag (getkword (if IsRus "\nСохранить координаты в [Файл/Excel/Не сохранять] <Файл> : "
"\nSave coordinates to [Text file/Excel/Not save] <Text> : ")))
(if(null sFlag)(setq sFlag "Text"))
(cond ((and (= "Text" sFlag)(setq filPath
       (getfiled (if IsRus "Сохранение координат в текстовый файл" "Save Coordinates to Text File") "Coordinates.txt" "txt;csv" 33)))
       (setq cFile(open filPath "w"))(foreach ln ptLst (write-line (strcat (rtos(car ln))","(rtos(cadr ln))
         (if(= 3(length ln))(strcat ","(rtos(nth 2 ln))))) cFile))(close cFile)(initget "Yes No")
       (setq oFlag(getkword (if IsRus "\nОткрыть файл? [Yes/No] <No> : " "\nOpen text file? [Yes/No] <No> : " )))
       (if(= oFlag "Yes")(startapp "notepad.exe" filPath))); end condition #1
     ((= "Excel" sFlag)(xls (mapcar '(lambda(x)(mapcar 'rtos x)) ptLst) nil nil "COOR")); end condition #2
     (t nil)))) (princ)); end of c:COOR

(defun c:COORT(/ cFile curPt filPath objSet oFlag oldMode ptLst sFlag lw isRus txtList buf pat)
(defun group-by-num (lst num / ls ret)(if (= (rem (length lst) num ) 0)(progn (setq ls nil)
  (repeat (/ (length lst) num)(repeat num (setq ls(cons (car lst) ls)lst (cdr lst)))
  (setq ret (append ret (list (reverse ls))) ls nil)))) ret)
(defun PtCollect(SelSet)(mapcar 'cdr (mapcar '(lambda(x)(assoc 10 x))(mapcar 'entget
(vl-remove-if 'listp(mapcar 'cadr(ssnamex SelSet))))))); end of PtCollect
(defun PLCollect(SelSet / ret)
(foreach lw (mapcar 'vlax-ename->vla-object (vl-remove-if 'listp(mapcar 'cadr(ssnamex SelSet))))
  (cond ((wcmatch (vla-get-ObjectName lw) "*Polyline")
	(setq ret (append ret (group-by-num (vlax-get lw 'Coordinates)
	(if (=(vla-get-ObjectName lw) "AcDbPolyline") 2 3)))))
       ((=(vla-get-ObjectName lw) "AcDbSpline")(setq ret (append ret (group-by-num
         (vlax-safearray->list(vlax-variant-value (vla-get-controlpoints lw)))  3))))
      (t nil))) ret)
  (vl-load-com)(setq isRus(= (getvar "SysCodePage") "ANSI_1251"))(if(not ptcol:mode)(setq ptcol:mode "Pick"))
  (initget "Указать Точка Блоки Полилиния Pick pOints Blocks poLyline _Pick pOints Blocks poLyline Pick pOints Blocks poLyline")
(setq oldMode ptcol:mode ptcol:mode
(getkword (if IsRus (strcat "\nВыберите режим [Указать/Точка/Блоки/Полилиния или сплайн] <"
(cadr (assoc ptcol:mode '(("Pick" "Указать")("pOints" "Указать")("Blocks" "Блоки")("poLyline" "Полилиния")))) ">: ")
	      (strcat "\nSpecify mode [Pick/pOints/Blocks/poLyline or spline] <"ptcol:mode">: "))) ptLst nil)
(if(null ptcol:mode)(setq ptcol:mode oldMode))
(cond ((= "Pick" ptcol:mode)(setq curPt T)
       (while curPt (setq curPt(getpoint (if IsRus
         "\nУкажите точку или Enter завершения > " "\nPick point or Enter to continue > ")))
	(if curPt (setq ptLst(append ptLst(list (trans curPt 1 0))))))); end condition #1
      ((= "pOints" ptcol:mode)(if (not(setq objSet(ssget "_I" '((0 . "POINT")))))(progn
         (if IsRus (princ "\nВыберите точки и нажмите Enter ")(princ "\nSelect points and press Enter "))
	 (setq objSet(ssget '((0 . "POINT"))))))(if objSet (setq ptLst(PtCollect objSet)))); end condition #2
      ((= "Blocks" ptcol:mode)(if (not(setq objSet(ssget "_I" '((0 . "INSERT")))))(progn
        (if IsRus(princ "\nВыберите блоки и нажмите Enter ")(princ "\nSelect blocks and press Enter "))
	   (setq objSet(ssget '((0 . "INSERT"))))))(if objSet (setq ptLst(PtCollect objSet)))); end condition #3
      ((= "poLyline" ptcol:mode)(if (not(setq objSet(ssget "_I" '((0 . "*POLYLINE,SPLINE")))))(progn
        (if IsRus(princ "\nВыберите полилинии и нажмите Enter  ")(princ "\nSelect polyline and press Enter "))
	   (setq objSet(ssget '((0 . "*POLYLINE,SPLINE"))))))(if objSet (setq ptLst(PLCollect objSet)))); end condition #4
); end cond
(if ptLst
  (progn
    (setq objSet(ssget "_X" (list '(0 . "*TEXT")(cons 410 (getvar "CTAB")))))
    (setq lw (vl-remove-if 'listp(mapcar 'cadr(ssnamex objSet))))
    (setq lw (mapcar '(lambda(x)(setq x (entget x))(list (cdr(assoc 10 x))(cdr(assoc 1 x)))) lw))
    (foreach pt ptlst
      (setq buf (mapcar '(lambda(x)(list (distance pt (car x))(cadr x))) lw))
      (setq pat (car buf))
      (foreach dst buf (if (< (car dst) (car pat))(setq pat dst)))
      (setq txtList (cons (cadr pat) txtList))
      )
    (setq txtList (reverse txtList))
    (princ "\n+++++++ Coordinates list +++++++\n")
    (setq ptLst (mapcar '(lambda (x) (trans x 0 1)) ptLst))
    (setq buf
    (mapcar '(lambda (x y)
               (princ (strcat "\n" y "  "
                              (rtos (car x))
                              ","
                              (rtos (cadr x))
                              (if (= 3 (length x))
                                (strcat "," (rtos (nth 2 x)))
                                ""
                              ) ;_ end of if
                      ) ;_ end of strcat
               ) ;_ end of princ
              (list y (rtos (car x))(rtos (cadr x))
                              (if (= 3 (length x))(rtos (nth 2 x))) ;_ end of if
                      )
             ) ;_ end of lambda
            ptLst txtList
    );_ end mapcar
          )
    (princ "\n\n+++++++++ End of list +++++++++")
    (initget
      "Файл Excel Не Text Excel Not _Text Excel Not Text Excel Not"
    ) ;_ end of initget
    (setq sFlag
           (getkword
             (if IsRus
               "\nСохранить координаты в [Файл/Excel/Не сохранять] <Файл> : "
               "\nSave coordinates to [Text file/Excel/Not save] <Text> : "
             ) ;_ end of if
           ) ;_ end of getkword
    ) ;_ end of setq
    (if (null sFlag)
      (setq sFlag "Text")
    ) ;_ end of if
    (cond ((and (= "Text" sFlag)
                (setq filPath
                       (getfiled (if IsRus
                                   "Сохранение координат в текстовый файл"
                                   "Save Coordinates to Text File"
                                 ) ;_ end of if
                                 "Coordinates.txt"
                                 "txt;csv"
                                 33
                       ) ;_ end of getfiled
                ) ;_ end of setq
           ) ;_ end of and
           (setq cFile (open filPath "w"))
           (foreach ln buf
             (write-line
               (apply 'strcat
               (append (list(car ln))
                       (mapcar '(lambda(x)(strcat "," x))
                               (cdr ln)
                               )
                       )
                 )     
               cFile
             ) ;_ end of write-line
           ) ;_ end of foreach
           (close cFile)
           (initget "Yes No")
           (setq oFlag (getkword (if IsRus
                                   "\nОткрыть файл? [Yes/No] <No> : "
                                   "\nOpen text file? [Yes/No] <No> : "
                                 ) ;_ end of if
                       ) ;_ end of getkword
           ) ;_ end of setq
           (if (= oFlag "Yes")
             (startapp "notepad.exe" filPath)
           ) ;_ end of if
          )                                       ; end condition #1
          ((= "Excel" sFlag)
           (xls buf
                '("Номер точки" "X" "Y" "Z")
                nil
                "COORM"
           ) ;_ end of xls
          )                                       ; end condition #2
          (t nil)
    ) ;_ end of cond
  ) ;_ end of progn
) ;_ end of if
 (princ))
(princ "\nType COOR, COORN or COORT in command line")