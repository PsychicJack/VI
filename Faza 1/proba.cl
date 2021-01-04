(defun main()
    
    (princ "Unesite velicinu (4/6): ")
    (setq velicinaTabele (read))

 

    (setq trentuniIgrac 'H)
    (setq trenutnaBoja 'X)
    (odabirPrvogIgraca)
    (odabirBoje)
  
 
    (setq tabela (inicijalizujTabelu '0))
    (setq tabelaZaIspitivanje (copy-tree tabela))
    
 

    ;;(let ((tabelaZaIspitivanje tabela)))
    (format t "~%")
    (potez tabela)
    (potpunPrikaz tabela)
)

 

(defun odabirPrvogIgraca () 
  (princ "Izbor prvog igraca (H/C): ")
  (setq prviIgracTemp (read))
  (if 
     (or (equal prviIgracTemp 'C) (equal prviIgracTemp 'H)) 
     (setf trentuniIgrac prviIgracTemp) 
    (princ "Ulaz mora biti H ili C"))                       
 )

 

(defun odabirBoje () 
  (princ "Izbor boje prvog igraca (X/O): ")
  (setq bojaIgracaTemp (read))

 

  (if 
      (or (equal bojaIgracaTemp 'X) (equal bojaIgracaTemp 'O)) 
      (setf trenutnaBoja bojaIgracaTemp) 
      (princ "Ulaz mora biti X ili O"))                       
)

 

(defun inicijalizujTabelu (rbrReda)
    (cond
        ((>= rbrReda velicinaTabele) '())
        (t
            (cons  (inicijalizujRed rbrReda velicinaTabele) 
                (inicijalizujTabelu (+ 1 rbrReda))
            )
        )
     ) 
)
(defun inicijalizujRed (rbrReda rbrKolone)
    (cond 
        ((<= rbrKolone 0) '())
        (t (if (= 4 velicinaTabele)
                 (cons (list '- '- '- '-) (inicijalizujRed rbrReda (- rbrKolone 1))) 
                 (if (= 6 velicinaTabele)
                (cons (list '- '- '- '- '- '-) (inicijalizujRed rbrReda (- rbrKolone 1)))
                (format t "Nije uneta ispravna vrednost za velicinu kocke")
            ))
             )
    )
)

 
 (list #\a #\b )
(defun potpunPrikaz (tab)
    (cond ((= velicinaTabele 4) (format t "0123456789ABCDEF")) ((= velicinaTabele 6) (format t "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ")))
    (format t "~%")
    (prikazStanja velicinaTabele velicinaTabele 0 velicinaTabele tab)
    (cond ((= velicinaTabele 4) (format t "0123456789ABCDEF")) ((= velicinaTabele 6) (format t "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ")))
)

 

(defun prikazStanja (od do ind1 ind2 tab)
    (decf ind2)
    (cond ((= ind2 -1) (setq ind2 0) (incf ind1)))
    (decf od)
    (cond ((= od -1) (setq od 0) (decf do)))
    (cond ((= do 0) '()) (t (stampajRed od do 0 ind1 ind2 tab) (format t "~%") (prikazStanja od do ind1 ind2 tab)))
)
 
(defun stampajRed (od do count ind1 ind2 tab)

 

    (cond ((= count velicinaTabele) '()) 
    (t  (stampajBlankoOd od '0) 
        (stampajPolja od do count (1- ind1) (1- ind2) tab)
        (stampajBlankoDo do velicinaTabele)
        
        (stampajRed od do (1+ count) ind1 ind2 tab)
    )
    )
)
 
(defun stampajBlankoOd (od trenutni)
    (cond ((= od trenutni) '()) (t (format t " ") (stampajBlankoOd od (1+ trenutni))))
)

 

(defun stampajPolja (od do red ind1 ind2 tab)
    (incf ind1)
    (incf ind2)
    (cond ((= od do) '())
    (t (princ (nth ind2 (nth ind1 (nth red tab)))) (stampajPolja (1+ od) do red ind1 ind2 tab))
    )
)

 

(defun stampajBlankoDo (stampajDo granica)
    (cond ((= stampajDo granica) '()) (t (format t " ") (stampajBlankoDo (1+ stampajDo) granica))) 
)

 

(defun potez (tabelaZaIspitivanje)
  
   
    (format t "~%Potez: ")
    (setq potez (read))
    (proveriPotez (write-to-string potez) tabelaZaIspitivanje)
  
    (potpunPrikaz tabela)
    (potpunPrikaz tabelaZaIspitivanje)
    (cond ((jeKrajIgre 0 0) (potpunPrikaz)(format t "~%Kraj igre.")) 
    (t '())
    )
)

 

(defun proveriPotez (potez tabelaZaIspitivanje)
    (setq temp (- (char-code (char potez 0)) 55)) 
    (cond ((< temp 10) (incf temp 7)))
    (setq rbrReda (floor temp velicinaTabele))
    (setq rbrStubica (mod temp velicinaTabele))
    (cond ((or (>= rbrReda velicinaTabele) (< rbrReda 0)) (format t "Nepravilan potez ~%") '())
        (t 
         (cond ((equal (nth rbrStubica (nth 0 (nth rbrReda tabelaZaIspitivanje))) '-) 
               
                (odigrajPotez rbrReda rbrStubica (- velicinaTabele 1) tabelaZaIspitivanje) (promeniIgraca) t)
          
               (t (format t "Stubic je popunjen ~%") '()))
        )
          )
    
)

 

(defun odigrajPotez (rbrReda rbrStubica trenutni tabelaZaIspitivanje)
    (cond ((equal  (nth rbrStubica (nth trenutni (nth rbrReda tabelaZaIspitivanje))) '-)
           (setf  (nth rbrStubica (nth trenutni (nth rbrReda tabelaZaIspitivanje))) trenutnaBoja)
           )
     (t (odigrajPotez rbrReda rbrStubica (1- trenutni) tabelaZaIspitivanje) )
          )
  (let ((tabela tabelaZaIspitivanje)))
)

 

(defun promeniIgraca () 
    (cond ((equal trentuniIgrac 'H) (setq trentuniIgrac 'C)) ((equal trentuniIgrac 'C) (setq trentuniIgrac 'H)))
    (cond ((equal trenutnaBoja 'X) (setq trenutnaBoja 'O)) ((equal trenutnaBoja 'O) (setq trenutnaBoja 'X)))
)

 


(defun jeKrajIgre (rbrReda rbrStubica)
    (cond ((>= rbrStubica velicinaTabele) (setq rbrStubica 0) (incf rbrReda)))
    (cond ((>= rbrReda velicinaTabele) t) (t 
    (cond ((not (equal (nth rbrStubica (nth 0 (nth rbrReda tabela))) '-)) (and t (jeKrajIgre rbrReda (1+ rbrStubica))))
            (t '())
    )))
    
)

 

(main)