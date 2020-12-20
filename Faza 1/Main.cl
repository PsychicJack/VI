
(defun main()
    
    (princ "Unesite velicinu (4/6): ")
    (setq velicinaTabele (read))

    (setq trentuniIgrac 'H)
    (setq trenutnaBoja 'X)
    (odabirPrvogIgraca)
    (odabirBoje)

    ;;(setq velicinaTabele 4)
    (setq tabela (inicijalizujTabelu '0))
    ;;(princ "---------")

    (format t "~%")
    (potez)
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
(defun inicijalizujRed (rbrReda rbrKolone);; promena iz '(- - - -) -> (list '- '- '- '-)
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

(defun potpunPrikaz () ;; prikaz tabela sa prikazom numeracije polja
    (cond ((= velicinaTabele 4) (format t "0123456789ABCDEF")) ((= velicinaTabele 6) (format t "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ")))
    (format t "~%")
    (prikazStanja velicinaTabele velicinaTabele 0 velicinaTabele)
    (cond ((= velicinaTabele 4) (format t "0123456789ABCDEF")) ((= velicinaTabele 6) (format t "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ")))
)

(defun prikazStanja (od do ind1 ind2) ;; radi prika tabela
    (decf ind2)
    (cond ((= ind2 -1) (setq ind2 0) (incf ind1)))
    (decf od)
    (cond ((= od -1) (setq od 0) (decf do)))
    (cond ((= do 0) '()) (t (stampajRed od do 0 ind1 ind2) (format t "~%") (prikazStanja od do ind1 ind2)))
)
 
(defun stampajRed (od do count ind1 ind2) ;; stampa ceo red na prikazu, ne red u listi (npr. za prvi red "   -   -   -   -", za drugi "  --  --  --  --")

    (cond ((= count velicinaTabele) '()) 
    (t  (stampajBlankoOd od '0) 
        (stampajPolja od do count (1- ind1) (1- ind2))
        (stampajBlankoDo do velicinaTabele)
        
        (stampajRed od do (1+ count) ind1 ind2)
    )
    )
)
 
(defun stampajBlankoOd (od trenutni) ;; stampa razmake sa leve strane reda matrice
    (cond ((= od trenutni) '()) (t (format t " ") (stampajBlankoOd od (1+ trenutni))))
)

(defun stampajPolja (od do red ind1 ind2)
    (incf ind1)
    (incf ind2)
    (cond ((= od do) '())
    (t (princ (nth ind2 (nth ind1 (nth red tabela)))) (stampajPolja (1+ od) do red ind1 ind2))
    )
)

(defun stampajBlankoDo (stampajDo granica) ;; stampa razmake sa desne strane reda matrice
    (cond ((= stampajDo granica) '()) (t (format t " ") (stampajBlankoDo (1+ stampajDo) granica))) 
)

(defun potez ()
    (potpunPrikaz)
    (format t "~%Potez: ")
    (setq potez (read))
    (proveriPotez (write-to-string potez));; write-to-string je obican convert to string, jer proveri potez radi samo sa stringovim
    (cond ((jeKrajIgre 0 0) (potpunPrikaz)(format t "~%Kraj igre.")) 
    (t (potez)))
)

(defun proveriPotez (potez) ;; proverava da li je potez validan i odigrava ga
    (setq temp (- (char-code (char potez 0)) 55));; uzima prvi element iz string potez i odredjuje njegov char code. Oduzima 55 jer karakter A ima kod 65, a nama predstavlja broj 10. 
    (cond ((< temp 10) (incf temp 7)));; posto postoji 7 mesta izmedju velikih slova i brojeva u ascii tabeli ukoliko je broj dodajemo mu 7
    (setq rbrReda (floor temp velicinaTabele));; odredjujemo rbrReda celobrojnim deljenjem
    (setq rbrStubica (mod temp velicinaTabele))
    (cond ((or (>= rbrReda velicinaTabele) (< rbrReda 0)) (format t "Nepravilan potez ~%") '())
        (t 
            (cond ((equal (nth rbrStubica (nth 0 (nth rbrReda tabela))) '-) (odigrajPotez rbrReda rbrStubica (- velicinaTabele 1)) (promeniIgraca)) (t (format t "Nepravilan potez ~%") '()))
        )
    )
)

(defun odigrajPotez (rbrReda rbrStubica trenutni) ;;ne proverava da li ima slobodnih mesta, jer je to provereno u proveriPotez
    (cond ((equal  (nth rbrStubica (nth trenutni (nth rbrReda tabela))) '-)
    (setf  (nth rbrStubica (nth trenutni (nth rbrReda tabela))) trenutnaBoja))
     (t (odigrajPotez rbrReda rbrStubica (1- trenutni)))
    )
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

;;(nth ind2 (nth ind1 (nth red matrica)))
;;(princ (- (char-code (char "A" 0)) 55))


;; 1 -> od = 3, do = 4, (0, 3)
;; 2 -> od = 2, do = 4, (0, 2) (1, 3)
;; 3 -> od = 1, do = 4, (0, 1) (1, 2) (2, 3)
;; 4 -> od = 0, do = 4, (0, 0) (1, 1) (2, 2) (3, 3)
;; 5 -> od = 0, dp = 3, (1, 0) (2, 1) (3, 2)
;; 6 -> od = 0, do = 2, (2, 0) (3, 1)
;; 7 -> od = 0, do = 1, (3, 0)