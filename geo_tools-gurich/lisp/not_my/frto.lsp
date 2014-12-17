(defun c:frto(/ ACTDOC COPOBJ ERRCOUNT EXTLST
       EXTSET FROMCEN LAYCOL MAXPT CURLAY
       MINPT OBJLAY OKCOUNT OLAYST
       SCLAY TOCEN TOOBJ VLAOBJ *ERROR* ASK)
  (vl-load-com)
  (defun *ERROR*(msg)
    (if olaySt (vla-put-Lock objLay olaySt)); end if
    (vla-EndUndoMark actDoc)(princ)); end of *ERROR*

  (defun GetBoundingCenter(vlaObj / blPt trPt cnPt)
  (vla-GetBoundingBox vlaObj 'minPt 'maxPt)
      (setq blPt(vlax-safearray->list minPt)
      trPt(vlax-safearray->list maxPt)
      cnPt(vlax-3D-point
      (list
            (+(car blPt)(/(-(car trPt)(car blPt))2))
            (+(cadr blPt)(/(-(cadr trPt)(cadr blPt))2))
            (+(caddr blPt)(/(-(caddr trPt)(caddr blPt))2)) ;_<<< Заменили
            )))); end of GetBoundingCenter
  (setq extSet(ssget "_I"))
 (while (not (setq toObj(entsel "\n+++ Select source object -> ")))
   (princ "\nSource objects isn't selected!"))
  (if(not extSet)
    (progn
      (princ "\n+++ Select destination objects and press Enter <- ")
      (setq extSet(ssget "_:L")))); end if
  (if(not extSet)(princ "\nDestination objects isn't selected!")); end if
  (if (and extSet toObj)
    (progn
      (initget "Yes No")
      (setq ask (getkword "\nRemove destination object [Yes/No] <No>:"))
      (setq actDoc (vla-get-ActiveDocument(vlax-get-Acad-object))
      layCol (vla-get-Layers actDoc)
      extLst (mapcar 'vlax-ename->vla-object (vl-remove-if 'listp
                     (mapcar 'cadr(ssnamex extSet))))
      vlaObj (vlax-ename->vla-object(car toObj))
      objLay (vla-Item layCol (vla-get-Layer vlaObj))
      olaySt (vla-get-Lock objLay)
     fromCen (GetBoundingCenter vlaObj)
      errCount 0  okCount 0); end setq
      (vla-StartUndoMark actDoc)
      (foreach obj extLst
        (setq toCen (GetBoundingCenter obj)
              scLay (vla-Item layCol (vla-get-Layer obj)));end setq
  (if(/= :vlax-true(vla-get-Lock scLay))
    (progn
    (setq curLay(vla-get-Layer obj))
    (vla-put-Lock objLay :vlax-false)
    (setq copObj(vla-copy vlaObj))
    (vla-Move copObj fromCen toCen)
    (_kpblc-ent-properties-copy obj copObj)
    (vla-put-Layer copObj curLay)
    (vla-put-Lock objLay olaySt)
    (if (= ask "Yes")(vla-Delete obj))
    (setq okCount(1+ okCount))
    ); end progn
    (setq errCount(1+ errCount))
    ); end if
  ); end foreach
      (princ (strcat "\n" (itoa okCount) " were changed. "
    (if(/= 0 errCount)(strcat (itoa errCount) " were on locked layer! ")  "")))
      (vla-EndUndoMark actDoc)); end progn
    (princ "\nSource object isn't selected! ")
    ); end if
  (princ)); end of c:frto
;|=============================================================================
*    Функция копирования настроек примитивов
*    Параметры вызова:
*   source   примитив-источник (vla)
*   dest   примитив-получатель (vla)
*    Выполняется копирование всех настроек (кроме точек, координат и т.п.), если
* это возможно. Копирование радиусов дуг и окружностей не выполняется.
*    Контроль и преобразование параметров не выполняется.
*    Примеры вызова:
(_kpblc-ent-properties-copy (vlax-ename->vla-object (car (entsel))) (vlax-ename->vla-object (car (entsel))))
*    URL http://www.arcada.com.ua/forum/viewtopic.php?t=504&start=15
=============================================================================|;
(defun _kpblc-ent-properties-copy (source dest)
 (foreach prop   '("Angle" "Layer" "Linetype" "LinetypeScale" "Lineweight"
        "Normal" "PlotStyleName" "Thickness" "Color" "Visible"
        "Closed" ;|"ConstantWidth" ; не копируется|; "Elevation" "LinetypeGeneration"
        "LinetypeScale" ;|"StartAngle" "EndAngle" ; не копируются|; "Alignment"
        "Backward" "Height" "ObliqueAngle" "Rotation" "ScaleFactor" "StyleName"
        "TextGenerationFlag"  "TextHeight"  "UpsideDown"  "AttachmentPoint" "BackgroundFill"
        "DrawingDirection"  "LineSpacingDistance" "LineSpacingFactor" "LineSpacingStyle"  "Width"
        "XScaleFactor" "YScaleFactor" "ZScaleFactor" ;| Viewport|; "ArcSmoothness" "CustomScale"
        "Direction" "DisplayLocked"  "GridOn" "LensLength" "ModelView" "ShadePlot" "SheetView"
        "SnapBasePoint" "SnapOn" "SnapRotationAngle" "StandardScale" "Target"  "TwistAngle"
        "UCSIconAtOrigin"   "UCSIconOn"     "UCSPerViewport" "ViewportOn")
 (if (and (vlax-property-available-p source prop)(vlax-property-available-p dest prop t))
  (_kpblc-error-catch
    '(lambda ()(vlax-put-property dest prop (vlax-get-property source prop))) nil)))) ;_ end of defun
;|=============================================================================
*    Оболочка отлова ошибок.
*    Параметры вызова:
*   protected-function   — "защищаемая" функция
*   on-error-function   — функция, выполняемая в случае ошибки
*    URL http://www.arcada.com.ua/forum/viewtopic.php?t=504&start=15
=============================================================================|;
(defun _kpblc-error-catch
       (protected-function on-error-function / catch_error_result)
  (setq catch_error_result (vl-catch-all-apply protected-function))
  (if (and (vl-catch-all-error-p catch_error_result) on-error-function)
    (apply on-error-function
      (list (vl-catch-all-error-message catch_error_result)))
    catch_error_result)) ;_ end of defun
;;;(princ "\nType FRTO in command line")

