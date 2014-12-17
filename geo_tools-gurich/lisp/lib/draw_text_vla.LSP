(defun draw_text_vla ;
       ; создание примитива Text средствами ActiveX
       ;пример (draw_text_vla active_document "текст" '(0 0 0) 2.5 T T nil T T)
       (spece TextString Point Height Alignment Rotation Linetype Layer Lineweight / text)
  (load_global_variable)
  (setq text (vla-AddText spece TextString (vlax-3D-point Point) Height))
  ; при Alignment - T то как будто не задавалось, если nil то =4 (сЕредина) и вставляется TextAlignmentPoint
  ; иначе значение используется как Alignment
  (if Alignment
    (if (not (eq (type Alignment) 'SYM))
      (progn
	(vla-put-Alignment text Alignment)
	(vla-put-TextAlignmentPoint text (vlax-3D-point Point))
	)
      )
    (progn
      (vla-put-Alignment text 4)
      (vla-put-TextAlignmentPoint text (vlax-3D-point Point))
      )
    )
  ; при Rotation - nil тип линии становится (/ pi 2) при T по умолчанию - 0
  ; при остальных значениях тип линии берется из этой переменной
  (if Rotation
    (if (not (eq (type Rotation) 'SYM))
      (vla-put-Rotation text Rotation)
      )
    (vla-put-Rotation text (/ pi 2))
    )
  ; при Linetype - nil тип линии становится "Continuous" при T текущий
  ; при остальных значениях тип линии берется из этой переменной
  (if Linetype
    (if (not (eq (type Linetype) 'SYM))
      (vla-put-Linetype text Linetype)
      )
    (vla-put-Linetype text "Continuous")
    )
  ; при Layer - nil слой становится "Ситуация" при T текущий
  ; при остальных значениях тип линии берется из этой переменной
  (if Layer
    (if (not (eq (type Layer) 'SYM))
      (vla-put-Layer text Layer)
      )
    (vla-put-Layer text "Ситуация")
    )
  ; при Lineweight - nil толщина становится 0,3 при T текущий
  ; при остальных значениях тип линии берется из этой переменной
  (if Lineweight
    (if (not (eq (type Lineweight) 'SYM))
      (vla-put-Lineweight text Lineweight)
      )
    (vla-put-Lineweight text 30)
    )
  text
  )
