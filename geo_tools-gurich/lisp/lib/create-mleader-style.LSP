(defun create-mleader-style ( mleaderstylename  / mldrdict newldrstyle textcolor leadercolor objcolor dimblk)
  ;;; �������� ����� ������� MIP
  ;;;   mleaderstylename - ��� �����
  ;;;   dimblk - ��� ����� �� ����� (�� ���������� DIMBLK, �������� _Dot) ��� nil - ���
  ;;; ���������� ��������� �� ������ ���������� �����
  ;;;; ������:   (mip-mleader-style-create "Skkk" nil)
  ;;; ���������� ����� �������
  ;;; (setvar "CMLEADERSTYLE" (vla-get-name (mip-mleader-style-create "Skkk" nil)))
  
  
  ;;; �������� ������� �����
  ;;;(setq mleaderstylename "MIP")
  ;;;  (if (and (getvar "CMLEADERSTYLE") ;;;������� ������������ �������������
  ;;;         (setq tb-dic(DICTSEARCH (NAMEDOBJDICT) "ACAD_MLEADERSTYLE"))
  ;;;	 (not (member (cons 3 (strcase mleaderstylename))(DICTSEARCH (NAMEDOBJDICT) "ACAD_MLEADERSTYLE"))))
  ;;;    (progn )
  ;;;    )
  (if (not (member (cons 3 mleaderstylename)(DICTSEARCH (NAMEDOBJDICT) "ACAD_MLEADERSTYLE")))
    (progn
;;;      (create-text-style "OTI" "ESKDU.shx")
      (setq dimblk nil)
      (or dimblk (setq dimblk "_None"))
      (setq mldrdict (vla-item (vla-get-dictionaries (vla-get-activedocument (vlax-get-acad-object))) "ACAD_MLEADERSTYLE"))
      (setq newldrstyle (vlax-invoke mldrdict 'addobject mleaderstylename "AcDbMLeaderStyle"))
      (setq textcolor acByBlock) ;;;���� ������
      (setq leadercolor acByBlock) ;;;���� �������
      (setq objcolor (vla-getinterfaceobject
		       (vlax-get-acad-object)
		       (strcat "AutoCAD.AcCmColor." (substr (getvar "acadver") 1 2))
		       ) ;_vla-getinterfaceobject
	    ) ;_setq
      (vla-put-ColorIndex objcolor textcolor)
      (vla-put-TextColor newldrstyle objcolor)
      (vla-put-ColorIndex objcolor leadercolor)
      (vla-put-LeaderLineColor newldrstyle objcolor)
      (if (not (tblobjname "block" dimblk))
	(progn
	  (setq textcolor (getvar "dimblk"))
	  (if (VL-CATCH-ALL-ERROR-P(VL-CATCH-ALL-APPLY 'setvar (list "dimblk" dimblk)))
	    (setvar "dimblk" (setq dimblk "_None"))
	    )
	  (setvar "dimblk" (if (= textcolor "") "." textcolor))
	  )
	)
      (foreach item
	       (list
		 '("AlignSpace" 5.0)
		 '("Annotative" 0)
		 '("ArrowSize" 1.0) ;_������ �������
		 (list "ArrowSymbol"  dimblk)
		 '("BitFlags" 0)
		 '("BlockConnectionType" 0)
		 '("BlockRotation" 0.0)
		 '("BlockScale" 1.0)
		 '("BreakSize" 0.125)
		 '("ContentType" 2)
		 '("Description"  "����� ��� ��������")
		 '("DoglegLength" 0.3)
		 '("DrawLeaderOrderType" 0)
		 '("DrawMLeaderOrderType" 1)
		 '("EnableBlockRotation" -1)
		 '("EnableBlockScale" -1)
		 '("EnableDogleg" -1)
		 '("EnableFrameText" 0)
		 '("EnableLanding" -1)
		 '("FirstSegmentAngleConstraint" 0)
		 '("LandingGap" 0.2)
		 '("LeaderLineType"  1)
		 ;;;	'("LeaderLineTypeId" "ByBlock") ;_ "Continuous"
		 '("LeaderLineTypeId" "Continuous") ;_ "ByBlock"
		 ;;;	     '("LeaderLineWeight" 30) ;;;��� ����� �������
		 '("MaxLeaderSegmentsPoints" 2)
		 '("ScaleFactor" 1.0)
		 '("SecondSegmentAngleConstraint"  0)
		 '("TextAlignmentType" 0)
		 '("TextAngleType" 0)
		 '("TextHeight" 1.5) ;_������ ������
		 '("TextLeftAttachmentType" 3)
		 '("TextRightAttachmentType" 3)
		 '("TextString"  "")
		 '("TextStyle" (getvar "TEXTSTYLE")) ;;;��������� ����� ������ ������������
		 )
	;;;(terpri)(princ item)
	(vlax-put-property newldrstyle (car item)(cadr item))
	)
      )
    newldrstyle
    )
  
  )