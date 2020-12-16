(setq prviIgrac 'H)
(defun odabirPrvogIgraca () 
  (princ "Izbor prvog igraca (H/C): ")
  (setq prviIgracTemp (read))
  (if 
     (or (equal prviIgracTemp 'C) (equal prviIgracTemp 'H)) 
     (setf prviIgrac prviIgracTemp) 
    (princ "Ulaz mora biti H ili C"))                       
 )
(setq bojaIgraca 'X)
(defun odabirBoje () 
  (princ "Izbor boje prvog igraca (X/O): ")
  (setq bojaIgracaTemp (read))

  (if 
      (or (equal bojaIgracaTemp 'X) (equal bojaIgracaTemp 'O)) 
      (setf bojaIgraca bojaIgracaTemp) 
      (princ "Ulaz mora biti X ili O"))                       
)


(odabirPrvogIgraca)
(odabirBoje)

