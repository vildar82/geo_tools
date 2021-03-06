;|============================================================================= 
 ******** ������ *************************************************************** 
 *    ������� ������� ����� ������. ���� ����� ������������ ��� ���� ������. 
 *    ��������� ������: 
 *   ��� 
 *    ������� ������: 
 (_kpblc-style-create-tablestyle)
 =============================================================================|; 
 (defun sample_kpblc-style-create-tablestyle (/ item *adoc*)
   (setq 
     item (vla-addobject 
       (vla-item (vla-get-dictionaries 
              (vla-get-activedocument (vlax-get-acad-object))
              );_ end of vla-get-dictionaries 
            "acad_tablestyle" 
            );_ end of vla-item 
       "SPDS" 
       "AcDbTableStyle" 
       );_ end of vla-AddObject 
     );_ end of setq 
   (vla-put-name item "SPDS")     ; ��� ����� 
   (vla-put-description item "������ ����"); �������� ����� 
   (vla-put-flowdirection item 0)  ; ����������� (?)
   (vla-put-horzcellmargin item 0.5)  ; �����.������ ������ �� ������� ������ 
   (vla-put-vertcellmargin item 0.5)  ; ����.������ ������ �� ������� ������ 
   (vla-settextheight 
     item 
     acdatarow 
     2.5 
     )              ; ������ ������ �� ��������� ��� datacells 
   (vla-settextheight 
     item 
     acheaderrow 
     3.5 
     )              ; ������ ������ ��� ����� ������� 
   (vla-settextheight 
     item 
     actitlerow 
     7 
     )              ; ������ ������ ��� ��������� ������� 
   (vla-settextstyle 
     item 
     (+ acdatarow acheaderrow actitlerow)
     "standard" 
     )              ; ����� ������ �� ��������� ��� ���� ����� 
   (vla-put-colorindex 
     (vla-getbackgroundcolor 
       item 
       (+ acdatarow acheaderrow actitlerow)
       );_ end of vla-GetBackgroundColor 
     256 
     )              ; ��������� ����� "������� ����" 
   ;;(vla-setbackgroundcolor item 3 1)
   (vla-setbackgroundcolornone item 7 :vlax-true)
   (vla-put-headersuppressed item :vlax-false)
   (vla-put-titlesuppressed item :vlax-false)
   (vla-setgridvisibility 
     item 
     (+ achorzinside acvertinside achorztop achorzbottom acvertleft acvertright)
     (+ acdatarow acheaderrow)
     :vlax-true 
     );_ end of vla-SetGridVisibility 
   (vla-setgridvisibility 
     item 
     (+ achorzinside acvertinside achorztop achorzbottom acvertleft acvertright)
     actitlerow 
     :vlax-false 
     );_ end of vla-SetGridVisibility 
   (vla-setgridlineweight 
     item 
     (+ achorzinside acvertinside)
     (+ acdatarow acheaderrow actitlerow)
     aclnwt025 
     );_ end of vla-SetGridLineWeight 
   (vla-setgridlineweight 
     item 
     (+ achorztop achorzbottom acvertright acvertleft)
     (+ acdatarow acheaderrow actitlerow)
     aclnwt050 
     );_ end of vla-SetGridLineWeight 
   );_ end of defun