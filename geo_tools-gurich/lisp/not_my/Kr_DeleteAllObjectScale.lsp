(defun C:Kr_DeleteAllObjectScale ( / ss pr cmd)
;��� ������� ������������� ������� ������������ ������� ���� ������������ ��������, ��� ���� ��� ��������� ������������� ���������.
  (vl-load-com)
  (if (setq ss (ssget"_X"))
    (progn
      (setq cmd (getvar "CMDECHO"))
      (setvar "CMDECHO" 0)
      (vl-cmdf "-objectscale" ss "" "_Add" (getvar "CANNOSCALE") "")
      (command)
	  (foreach pr (dictsearch (namedobjdict) "acad_scalelist")
	    (if (and (= (car pr) 350)
	             (not (eq (getvar "CANNOSCALE") (cdr (assoc 300 (entget (cdr pr))))))
		     )
	      (vl-cmdf "-objectscale" ss "" "_Delete" (cdr (assoc 300 (entget (cdr pr)))))
	    );if
	    (command)
	    (princ)
	    );foreach
      (setvar "CMDECHO" cmd)
      );progn
    );if
  (princ)
  );defun